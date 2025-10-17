library(testthat)

Rscript <- "HW09_ProgrammingPart02.R"


# ------------------------------------------------------------------------------
# Tests
# ------------------------------------------------------------------------------



test_that("script starts with metadata (visible)", {
  first_character <- substr(readLines(Rscript, n = 1), 1, 1)
  expect_equal(first_character, '#')
})




test_that("classify_number identifies negative numbers (visible)", {
  expect_equal(classify_number(runif(1, -100000, 0)), "Negative")
})

test_that("classify_number identifies zero (visible)", {
  expect_equal(classify_number(0), "Zero")
})

test_that("classify_number identifies zero (visible)", {
  expect_equal(classify_number(runif(1, 0, 10)), "Small")
})

test_that("classify_number identifies zero (visible)", {
  expect_equal(classify_number(10), "Large")
  expect_equal(classify_number(runif(1, 10,1e6)), "Large")
})




test_that("filter_even_positive returns correct values (visible)", {
  expect_equal(filter_even_positive(c(-2, 0, 3, 4, 6, -8)), c(4, 6))
  expect_equal(filter_even_positive(c(2, 4, 6)), c(2, 4, 6))
})

test_that("filter_even_positive returns empty numeric if no even, positive (visible)", {
  expect_equal(filter_even_positive(c(1, 3, 5)), numeric(0))  # No even positives
  expect_equal(filter_even_positive(numeric(0)), numeric(0))  # Empty input
})




test_that("assign_grade assigns As correctly (visible)", {
  expect_equal(assign_grade(90), "A")
  expect_equal(assign_grade(runif(1, 90, 100)), "A")
})

test_that("assign_grade assigns Bs correctly (visible)", {
  expect_equal(assign_grade(80), "B")
  expect_equal(assign_grade(runif(1, 80, 90)), "B")
})

test_that("assign_grade assigns Cs correctly (visible)", {
  expect_equal(assign_grade(70), "C")
  expect_equal(assign_grade(runif(1, 70, 80)), "C")
})

test_that("assign_grade assigns As correctly (visible)", {
  expect_equal(assign_grade(60), "D")
  expect_equal(assign_grade(runif(1, 60, 70)), "D")
})

test_that("assign_grade assigns As correctly (visible)", {
  expect_equal(assign_grade(runif(1, 0, 60)), "F")
})





test_that("simple_calculator adds correctly (visible)", {
  x <- rnorm(1); y <- rnorm(1)
  expect_equal(simple_calculator(x, y, "add"), x+y)
})

test_that("simple_calculator subtracts correctly (visible)", {
  x <- rnorm(1); y <- rnorm(1)
  expect_equal(simple_calculator(x, y, "subtract"), x-y)
})

test_that("simple_calculator multiplies correctly (visible)", {
  x <- rnorm(1); y <- rnorm(1)
  expect_equal(simple_calculator(x, y, "multiply"), x*y)
})

test_that("simple_calculator divides correctly (visible)", {
  x <- rnorm(1); y <- rnorm(1)
  expect_equal(simple_calculator(x, y, "divide"), x/y)
})

test_that("simple_calculator returns Error if invalid operation (visible)", {
  expect_error(simple_calculator(5, 3, "mod"), "Invalid operation")
})




test_that("count_conditions counts positives correctly (visible)", {
  result <- count_conditions(c(1, -2, 0, NA, 5, NA, -3))
  expect_equal(result$positives, 2)
})

test_that("count_conditions counts negatives correctly (visible)", {
  result <- count_conditions(c(1, -2, 0, NA, 5, NA, -3))
  expect_equal(result$negatives, 2)
})

test_that("count_conditions counts zeros correctly (visible)", {
  result <- count_conditions(c(1, -2, 0, NA, 5, NA, -3))
  expect_equal(result$zeros, 1)
})

test_that("count_conditions counts NAs correctly (visible)", {
  result <- count_conditions(c(1, -2, 0, NA, 5, NA, -3))
  expect_equal(result$nas, 2)
})





test_that("summarize_scores returns correct summary (visible)", {
  df <- data.frame(
    name = c("Alice", "Bob", "Charlie", "Diana", "Eli"),
    score = c(88, 92, 67, 75, 59),
    passed = c(TRUE, TRUE, FALSE, TRUE, FALSE)
  )
  
  result <- summarize_scores(df)
  
  expect_equal(result$num_students, 5)
  expect_equal(result$num_passed, 3)
  expect_equal(result$average_score, mean(c(88, 92, 67, 75, 59)))
  expect_equal(result$highest_score, "Bob")
})


test_that("summarize_scores handles NA values (visible)", {
  df <- data.frame(
    name = c("Alice", "Bob", "Charlie"),
    score = c(88, NA, 67),
    passed = c(TRUE, NA, FALSE)
  )

  result <- summarize_scores(df)

  expect_equal(result$num_students, 3)
  expect_equal(result$num_passed, 1)  # Only Alice passed
  expect_equal(result$average_score, mean(c(88, 67)))
  expect_equal(result$highest_score, "Alice")
})


test_that("summarize_scores works with empty data.frame (visible)", {
  df <- data.frame(name = character(0), score = numeric(0), passed = logical(0))
  
  result <- summarize_scores(df)
  
  expect_equal(result$num_students, 0)
  expect_equal(result$num_passed, 0)
  expect_true(is.nan(result$average_score))  # mean of empty vector is NaN
  expect_equal(result$highest_score, character(0))
})



test_that("summarize_by_group has correct parameters (visible)", {
    expect_equal(names(formals(summarize_by_group)),
                 c("df","var", "group"))
})

test_that("summarize_by_group output is a data.frame (visible)", {
  expect_true(is.data.frame(summarize_by_group(ToothGrowth, "len",    "supp")))
  expect_true(is.data.frame(summarize_by_group(ChickWeight, "weight", "Chick")))
})

test_that("summarize_by_group output has correct column names (visible)", {
  expect_setequal(colnames(summarize_by_group(ToothGrowth, "len", "supp")), 
                  c("supp", "n", "mean", "sd"))
  expect_setequal(colnames(summarize_by_group(ChickWeight, "weight", "Chick")), 
                  c("Chick", "n", "mean", "sd"))
})


test_that("summarize_by_group calculates n correctly (visible)", {
  expect_equal(summarize_by_group(ToothGrowth, "len", "supp")$n, c(30, 30))
  expect_equal(summarize_by_group(ChickWeight[ChickWeight$Chick %in% 1:3, ], 
                                  "weight", "Chick")$n,
               c(12, 12, 12))
})


test_that("summarize_by_group calculates mean correctly (visible)", {
  expect_equal(summarize_by_group(ToothGrowth, "len", "supp")$mean,
               c(20.66333, 16.96333), tolerance = .001)
})

test_that("summarize_by_group calculates sd correctly (visible)", {
  expect_equal(summarize_by_group(ToothGrowth, "len", "supp")$sd,
                  c(6.605561, 8.266029), tolerance = .001)
})





test_that("modify_plot has correct parameters (visible)", {
  expect_equal(names(formals(modify_plot)),
               c("g","log_x", "log_y", "x_label", "y_label", "title"))
})

test_that("modify_plot has correct output for log_x input (visible)", {
  g <- ggplot(data.frame(x = runif(10), y = runif(10)),
                         aes(x=x, y=y)) + 
                geom_point()
  g_log_x <- g + scale_x_log10()
  
  expect_equal(ggplot_build(modify_plot(g, log_x = TRUE))$layout$panel_scales_x[[1]]$trans$name, 
               "log-10")
  expect_equal(ggplot_build(modify_plot(g_log_x, log_x = FALSE))$layout$panel_scales_x[[1]]$trans$name, 
               "identity")
})


test_that("modify_plot has correct output for log_y input (visible)", {
  g <- ggplot(data.frame(x = runif(10), y = runif(10)),
              aes(x=x, y=y)) + 
    geom_point()
  g_log_y <- g + scale_y_log10()
  
  expect_equal(ggplot_build(modify_plot(g, log_y = TRUE))$layout$panel_scales_y[[1]]$trans$name, 
               "log-10")
  expect_equal(ggplot_build(modify_plot(g_log_y, log_y = FALSE))$layout$panel_scales_y[[1]]$trans$name, 
               "identity")
})


test_that("modify_plot has correct output for x_lab input (visible)", {
  g <- ggplot(data.frame(x = runif(10), y = runif(10)),
              aes(x=x, y=y)) + 
    geom_point()
  
  random_lab <- paste0(sample(LETTERS, 10), collapse = "")
  expect_equal(modify_plot(g, x_lab = random_lab)$labels$x, random_lab)
})

test_that("modify_plot has correct output for y_lab input (visible)", {
  g <- ggplot(data.frame(x = runif(10), y = runif(10)),
              aes(x=x, y=y)) + 
    geom_point()
  
  random_lab <- paste0(sample(LETTERS, 10), collapse = "")
  expect_equal(modify_plot(g, y_lab = random_lab)$labels$y, random_lab)
})


test_that("modify_plot has correct output for title input (visible)", {
  g <- ggplot(data.frame(x = runif(10), y = runif(10)),
              aes(x=x, y=y)) + 
    geom_point()
  
  random_lab <- paste0(sample(LETTERS, 10), collapse = "")
  expect_equal(modify_plot(g, title = random_lab)$labels$title, random_lab)
})
