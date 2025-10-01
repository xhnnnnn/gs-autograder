library(testthat)

answers <- readRDS("answers.RDS")
Rscript <- "HW06_TidyPart02.R"


expect_string_is_in_Rscript <- function(string) {
  test_that(paste(string, "is used (visible)"), {
    # Read the entire file into one string
    file_content <- Rscript |>
      readLines(warn = FALSE) |>
      paste(collapse = "\n")
    
    # Expect the string to be present
    expect_true(grepl(string, file_content, fixed=TRUE))
  }) 
}

# FIX: Merge global + layer aes without modifyList (robust for 'uneval')
.all_aes_vars <- function(p) {
  amap <- list()
  # global aes
  if (!is.null(p$mapping) && length(p$mapping)) {
    amap <- as.list(p$mapping)
  }
  # layer overrides (manual copy to avoid modifyList glitches on 'uneval')
  for (ly in p$layers) {
    if (!is.null(ly$mapping) && length(ly$mapping)) {
      lm <- as.list(ly$mapping)
      for (nm in names(lm)) amap[[nm]] <- lm[[nm]]
    }
  }
  # extract variable names (works for plain symbols like cut/mean)
  lapply(amap, function(e) {
    if (is.null(e)) character(0) else all.vars(rlang::get_expr(e))
  })
}


# FIX: assert that aes_name uses variable var (regardless of global vs layer)
expect_has_aes_var <- function(p, aes_name, var, msg = NULL) {
  vars <- .all_aes_vars(p)
  got  <- vars[[aes_name]]
  testthat::expect_true(identical(got, var),
    msg %||% sprintf("aes '%s' does not use variable '%s'%s",
                     aes_name, var,
                     if (is.null(got)) "" else sprintf(" (got: %s)", got)))
}


# ------------------------------------------------------------------------------
# Tests
# ------------------------------------------------------------------------------



test_that("script starts with metadata (visible)", {
  first_character <- substr(readLines(Rscript, n = 1), 1, 1)
  expect_equal(first_character, '#')
})

expect_string_is_in_Rscript("|>")

expect_string_is_in_Rscript("select")
test_that("diamonds_price_only is correct (visible)",{
  expect_equivalent(diamonds_price_only, answers$diamonds_price_only)
})

test_that("diamonds_x_to_z is correct (visible)",{
  expect_equivalent(diamonds_x_to_z, answers$diamonds_x_to_z)
})

test_that("diamonds_not_clarity is correct (visible)",{
  expect_equivalent(diamonds_not_clarity, answers$diamonds_not_clarity)
})


expect_string_is_in_Rscript("filter")
test_that("diamonds_color_J is correct (visible)",{
  expect_equivalent(diamonds_color_J, answers$diamonds_color_J)
})

test_that("diamonds_color_not_J is correct (visible)",{
  expect_equivalent(diamonds_color_not_J, answers$diamonds_color_not_J)
})

test_that("diamonds_color_I_or_J is correct (visible)",{
  expect_equivalent(diamonds_color_I_or_J, answers$diamonds_color_I_or_J)
})

test_that("diamonds_carat_greater_than_1 is correct (visible)",{
  expect_equivalent(diamonds_carat_greater_than_1, answers$diamonds_carat_greater_than_1)
})

test_that("diamonds_carat_0.5_to_1 is correct (visible)",{
  expect_equivalent(diamonds_carat_0.5_to_1, answers$diamonds_carat_0.5_to_1)
})

test_that("diamonds_carat1_cutFair is correct (visible)",{
  expect_equivalent(diamonds_carat1_cutFair, answers$diamonds_carat1_cutFair)
})


expect_string_is_in_Rscript("rename")
test_that("diamonds_Price is correct (visible)",{
  expect_equivalent(diamonds_Price, answers$diamonds_Price)
})


expect_string_is_in_Rscript("mutate")
test_that("diamonds_price_per_carat is correct (visible)",{
  expect_equivalent(diamonds_price_per_carat, answers$diamonds_price_per_carat)
})

test_that("diamonds_cut_character is correct (visible)",{
  expect_equivalent(diamonds_cut_character, answers$diamonds_cut_character)
})

test_that("diamonds_cut_now_character is correct (visible)",{
  expect_equivalent(diamonds_cut_now_character, answers$diamonds_cut_now_character)
})



expect_string_is_in_Rscript("summarize")
test_that("diamonds_summarized has n, mean, and sd columns (visible)",{
  expect_equivalent(sort(names(diamonds_summarized)),
                    sort(names(answers$diamonds_summarized)))
})

test_that("diamonds_summarized is correct (visible)",{
  expect_equivalent(diamonds_summarized, answers$diamonds_summarized)
})



expect_string_is_in_Rscript("group_by")
test_that("diamonds_summarized_by_color has color, n, mean, and sd columns (visible)",{
  expect_equivalent(sort(names(        diamonds_summarized_by_color)),
                    sort(names(answers$diamonds_summarized_by_color)))
})

test_that("diamonds_summarized_by_color has correct dimensions (visible)",{
  expect_equivalent(dim(        diamonds_summarized_by_color),
                    dim(answers$diamonds_summarized_by_color))
})

test_that("diamonds_summarized is correct (visible)",{
  expect_equivalent(        diamonds_summarized_by_color,
                    answers$diamonds_summarized_by_color)
})



test_that("diamonds_for_plot has correct dimensions (visible)", {
  expect_equivalent(dim(        diamonds_for_plot),
                    dim(answers$diamonds_for_plot))
})

test_that("diamonds_for_plot has correct column names (visible)", {
  expect_setequal(
    names(diamonds_for_plot),
    names(answers$diamonds_for_plot)
  )
})


test_that("diamonds_for_plot has correct means (visible)", {
  expect_equivalent(sort(        diamonds_for_plot$mean),
                    sort(answers$diamonds_for_plot$mean))
})


test_that("diamonds_for_plot has only SI1 and SI2 diamonds (visible)", {
  expect_true(all(diamonds_for_plot$Clarity %in% c("SI1", "SI2")))
})



##########################


expect_string_is_in_Rscript("ggplot")
print(plot)

test_that("plot is a jitterplot (visible)", {
# FIX: ggplot objects are lists (S3), not S4. Use $ instead of @
  expect_s3_class(plot$layers[[1]]$geom, "GeomPoint")
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
# FIX: Use $ instead of @ to access labels
  expect_equal(plot$labels$x, "Cut")
})

test_that("plot y-axis is 'Mean Price per Carat' (visible)", {
  # FIX: Use $ instead of @
  expect_equal(plot$labels$y, "Mean Price per Carat")
})

test_that("plot title is 'SI1 and SI2 Diamonds' (visible)", {
  # FIX: Use $ instead of @
  expect_equal(plot$labels$title, "SI1 and SI2 Diamonds")
})

test_that("plot uses correct theme (visible)", {
   # FIX: Use $ instead of @, and check element classes correctly
  panel_bg <- plot$theme$panel.background
  expect_s3_class(panel_bg, "element_rect")   # ggplot theme elements are "element_rect"
  expect_equal(panel_bg$fill, "white")
  
  # FIX: panel.grid.major is often element_blank(), not NULL
  panel_grid_major <- plot$theme$panel.grid.major
  expect_true(is.null(panel_grid_major) || inherits(panel_grid_major, "element_blank"))

})








