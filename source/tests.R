suppressPackageStartupMessages({ library(testthat) })
answers <- readRDS("answers.RDS")

# NEW: path of the student's script (absolute);
student_file <- Sys.getenv("GS_STUDENT_FILE", "HW05_TidyPart01.R")

test_that("script starts with metadata (visible)", {
  first_character <- substr(readLines(student_file, n = 1), 1, 1)
  expect_equal(first_character, '#')
})

test_that("Pipe operator, |>, is used (visible)", {
  file_content <- student_file |>
    readLines(warn = FALSE) |>
    paste(collapse = "\n")
  expect_true(grepl("|>", file_content, fixed = TRUE))
})

test_that("DNase_n is correct (visible)", {
  expect_equivalent(DNase_n, answers$DNase_n)
})

test_that("DNase_p is correct (visible)", {
  expect_equivalent(DNase_p, answers$DNase_p)
})

test_that("DNase_names is correct (visible)", {
  expect_equivalent(DNase_names, answers$DNase_names)
})

test_that("DNase_nlevels_Run is correct (visible)", {
  expect_equivalent(DNase_nlevels_Run, answers$DNase_nlevels_Run)
})

test_that("DNase_min_conc is correct (visible)", {
  expect_equivalent(DNase_min_conc, answers$DNase_min_conc)
})

test_that("DNase_mean_density is correct (visible)", {
  expect_equivalent(DNase_mean_density, answers$DNase_mean_density)
})

test_that("DNase_sd_density is correct (visible)", {
  expect_equivalent(DNase_sd_density, answers$DNase_sd_density)
})

# ------------------------------------------------------------------------------

test_that("women_n is correct (visible)", {
  expect_equivalent(women_n, answers$women_n)
})

test_that("women_p is correct (visible)", {
  expect_equivalent(women_p, answers$women_p)
})

test_that("women_names is correct (visible)", {
  expect_equivalent(women_names, answers$women_names)
})

test_that("women_median_height is correct (visible)", {
  expect_equivalent(women_median_height, answers$women_median_height)
})

test_that("women_variance_weight is correct (visible)", {
  expect_equivalent(women_variance_weight, answers$women_variance_weight)
})

test_that("women_height_weight_correlation is correct (visible)", {
  expect_equivalent(women_height_weight_correlation, answers$women_height_weight_correlation)
})

# ------------------------------------------------------------------------------

test_that("HW05data_n is correct (visible)", {
  expect_equivalent(HW05data_n, answers$HW05data_n)
})

test_that("HW05data_p is correct (visible)", {
  expect_equivalent(HW05data_p, answers$HW05data_p)
})

test_that("HW05data_names is correct (visible)", {
  expect_equivalent(HW05data_names, answers$HW05data_names)
})

test_that("HW05data_var1_nlevels is correct (visible)", {
  expect_equivalent(HW05data_var1_nlevels, answers$HW05data_var1_nlevels)
})

test_that("HW05data_var1_levels is correct (visible)", {
  expect_equivalent(HW05data_var1_levels, answers$HW05data_var1_levels)
})

test_that("HW05data_var2_mean is correct (visible)", {
  expect_equivalent(HW05data_var2_mean, answers$HW05data_var2_mean)
})

test_that("HW05data_var2_sd is correct (visible)", {
  expect_equivalent(HW05data_var2_sd, answers$HW05data_var2_sd)
})
