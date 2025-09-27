suppressPackageStartupMessages({ library(testthat) })

# Load the official answers from /autograder/source
answers <- readRDS(file.path("/autograder/source", "answers.RDS"))

# Get the student script path from environment variable
student_file <- Sys.getenv("GS_STUDENT_FILE", "")
stopifnot(nzchar(student_file), file.exists(student_file))

# Helper: check if a string is present in the student script
expect_string_is_in_Rscript <- function(string) {
  test_that(paste(string, "is used (visible)"), {
    file_content <- readLines(student_file, warn = FALSE) |>
      paste(collapse = "\n")
    expect_true(grepl(string, file_content, fixed = TRUE))
  })
}

# Helper: collect all aes variables from a ggplot object
.all_aes_vars <- function(p) {
  amap <- as.list(p$mapping)
  for (ly in p$layers) {
    amap <- utils::modifyList(amap, as.list(ly$mapping))
  }
  lapply(amap, function(e) if (is.null(e)) character(0) else all.vars(rlang::get_expr(e)))
}

# Null-coalescing operator
`%||%` <- function(a, b) if (is.null(a)) b else a

# Helper: test if a ggplot has a specific variable mapped to an aesthetic
expect_has_aes_var <- function(p, aes_name, var, msg = NULL) {
  vars <- .all_aes_vars(p)
  ok <- var %in% (vars[[aes_name]] %||% character(0))
  expect_true(ok, msg %||% sprintf("aes '%s' does not use variable '%s'", aes_name, var))
}

# --- Begin tests ---

test_that("script starts with metadata (visible)", {
  first_character <- substr(readLines(student_file, n = 1), 1, 1)
  expect_equal(first_character, '#')
})

expect_string_is_in_Rscript("|>")
expect_string_is_in_Rscript("select")

test_that("diamonds_price_only is correct (visible)", {
  expect_equivalent(diamonds_price_only, answers$diamonds_price_only)
})

test_that("diamonds_x_to_z is correct (visible)", {
  expect_equivalent(diamonds_x_to_z, answers$diamonds_x_to_z)
})

test_that("diamonds_not_clarity is correct (visible)", {
  expect_equivalent(diamonds_not_clarity, answers$diamonds_not_clarity)
})

expect_string_is_in_Rscript("filter")

test_that("diamonds_color_J is correct (visible)", {
  expect_equivalent(diamonds_color_J, answers$diamonds_color_J)
})

test_that("diamonds_color_not_J is correct (visible)", {
  expect_equivalent(diamonds_color_not_J, answers$diamonds_color_not_J)
})

test_that("diamonds_color_I_or_J is correct (visible)", {
  expect_equivalent(diamonds_color_I_or_J, answers$diamonds_color_I_or_J)
})

test_that("diamonds_carat_greater_than_1 is correct (visible)", {
  expect_equivalent(diamonds_carat_greater_than_1, answers$diamonds_carat_greater_than_1)
})

test_that("diamonds_carat_0.5_to_1 is correct (visible)", {
  expect_equivalent(diamonds_carat_0.5_to_1, answers$diamonds_carat_0.5_to_1)
})

test_that("diamonds_carat1_cutFair is correct (visible)", {
  expect_equivalent(diamonds_carat1_cutFair, answers$diamonds_carat1_cutFair)
})

expect_string_is_in_Rscript("rename")

test_that("diamonds_Price is correct (visible)", {
  expect_equivalent(diamonds_Price, answers$diamonds_Price)
})

expect_string_is_in_Rscript("mutate")

test_that("diamonds_price_per_carat is correct (visible)", {
  expect_equivalent(diamonds_price_per_carat, answers$diamonds_price_per_carat)
})

test_that("diamonds_cut_character is correct (visible)", {
  expect_equivalent(diamonds_cut_character, answers$diamonds_cut_character)
})

test_that("diamonds_cut_now_character is correct (visible)", {
  expect_equivalent(diamonds_cut_now_character, answers$diamonds_cut_now_character)
})

expect_string_is_in_Rscript("summarize")

test_that("diamonds_summarized has n, mean, and sd columns (visible)", {
  expect_equivalent(sort(names(diamonds_summarized)),
                    sort(names(answers$diamonds_summarized)))
})

test_that("diamonds_summarized is correct (visible)", {
  expect_equivalent(diamonds_summarized, answers$diamonds_summarized)
})

expect_string_is_in_Rscript("group_by")

test_that("diamonds_summarized_by_color has color, n, mean, and sd columns (visible)", {
  expect_equivalent(sort(names(diamonds_summarized_by_color)),
                    sort(names(answers$diamonds_summarized_by_color)))
})

test_that("diamonds_summarized_by_color has correct dimensions (visible)", {
  expect_equivalent(dim(diamonds_summarized_by_color),
                    dim(answers$diamonds_summarized_by_color))
})

test_that("diamonds_summarized_by_color is correct (visible)", {
  expect_equivalent(diamonds_summarized_by_color,
                    answers$diamonds_summarized_by_color)
})

test_that("diamonds_for_plot has correct dimensions (visible)", {
  expect_equivalent(dim(diamonds_for_plot),
                    dim(answers$diamonds_for_plot))
})

test_that("diamonds_for_plot has correct column names (visible)", {
  expect_equivalent(names(diamonds_for_plot),
                    names(answers$diamonds_for_plot))
})

test_that("diamonds_for_plot has correct means (visible)", {
  expect_equivalent(sort(diamonds_for_plot$mean),
                    sort(answers$diamonds_for_plot$mean))
})

test_that("diamonds_for_plot has only SI1 and SI2 diamonds (visible)", {
  expect_true(all(diamonds_for_plot$Clarity %in% c("SI1", "SI2")))
})

expect_string_is_in_Rscript("ggplot")

# Do not print(plot); only assert its structure
test_that("plot is a jitterplot (visible)", {
  expect_s3_class(plot$layers[[1]]$geom, "GeomPoint")
  pos <- plot$layers[[1]]$position
  expect_true(inherits(pos, "PositionJitter"),
              info = "First layer is not using position_jitter().")
})

test_that("plot has cut on x-axis (visible)", {
  expect_has_aes_var(plot, "x", "cut")
})

test_that("plot has mean on y-axis (visible)", {
  expect_has_aes_var(plot, "y", "mean")
})

test_that("plot has Color as color (visible)", {
  expect_has_aes_var(plot, "colour", "Color")
})

test_that("plot has Clarity as shape (visible)", {
  expect_has_aes_var(plot, "shape", "Clarity")
})

test_that("plot x-axis is 'Cut' (visible)", {
  expect_equal(plot$labels$x, "Cut")
})

test_that("plot y-axis is 'Mean Price per Carat' (visible)", {
  expect_equal(plot$labels$y, "Mean Price per Carat")
})

test_that("plot title is 'SI1 and SI2 Diamonds' (visible)", {
  expect_equal(plot$labels$title, "SI1 and SI2 Diamonds")
})

test_that("plot uses correct theme (visible)", {
  resolved <- ggplot2::ggplot_build(plot)$layout$theme
  panel_bg <- resolved$panel.background
  expect_true(inherits(panel_bg, "element_rect"))
  expect_true(panel_bg$fill %in% c("white", "#FFFFFF"))
  pgm <- resolved$panel.grid.major
  expect_true(is.null(pgm) || inherits(pgm, "element_blank"))
})
