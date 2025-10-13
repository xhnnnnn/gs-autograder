library(testthat)

Rscript <- "HW08_ProgrammingPart01.R"


# ------------------------------------------------------------------------------
# Tests
# ------------------------------------------------------------------------------



test_that("script starts with metadata (visible)", {
  first_character <- substr(readLines(Rscript, n = 1), 1, 1)
  expect_equal(first_character, '#')
})


# ------------------------------------------------------------------------------
# Object type
# ------------------------------------------------------------------------------
test_that("is_logical returns TRUE for logicals (visible)", {
  # Logicals
  expect_true(is_logical(TRUE))
  expect_true(is_logical(rep(FALSE, 2)))
  expect_true(is_logical(matrix(c(TRUE, FALSE), 1, 2)))
})

test_that("is_logical returns FALSE for non-logicals (visible)", {
  # Not logicals
  expect_false(is_logical(0))
  expect_false(is_logical(LETTERS))
  expect_false(is_logical(matrix(1:10, 2, 5)))
})


test_that("is_numeric returns TRUE for numerics (visible)", {
  # numerics
  expect_true(is_numeric(1.5))
  expect_true(is_numeric(rnorm(2)))
  expect_true(is_numeric(matrix(runif(10), 2, 5)))
})

test_that("is_numeric returns FALSE for non-numerics (visible)", {
  # Not numerics
  expect_false(is_numeric(TRUE))
  expect_false(is_numeric(LETTERS))
  expect_false(is_numeric(matrix(LETTERS[1:6], 2, 3)))
})


test_that("is_character returns TRUE for characters (visible)", {
  # characters
  expect_true(is_character("1.5"))
  expect_true(is_character(LETTERS[1:5]))
  expect_true(is_character(matrix(LETTERS[1:6], 2, 3)))
})

test_that("is_character returns FALSE for non-characters (visible)", {
  # Not characters
  expect_false(is_character(TRUE))
  expect_false(is_character(rnorm(2)))
  expect_false(is_character(matrix(runif(6), 2, 3)))
})

# ------------------------------------------------------------------------------
# Object dimension
# ------------------------------------------------------------------------------

test_that("vector_length returns correct length for scalars, vectors, and lists (visible)", {
  expect_equal(vector_length(1), 1)
  expect_equal(vector_length(rnorm(2)), 2)
  expect_equal(vector_length(LETTERS[1:6]), 6)
  expect_equal(vector_length(list(1,2)), 2)
})



test_that("matrix_dimension returns correct dimensions for matrices and data.frames (visible)", {
  expect_equal(matrix_dimension(matrix(NA, 1, 1)),                  c(1, 1))
  expect_equal(matrix_dimension(rbind(rnorm(2), runif(2))),         c(2, 2))
  expect_equal(matrix_dimension(cbind(LETTERS[1:3], LETTERS[4:6])), c(3, 2))
  expect_equal(matrix_dimension(ToothGrowth),                       c(60, 3))
})




# ------------------------------------------------------------------------------
# Functions
# ------------------------------------------------------------------------------

# convert_celsius_to_fahrenheit

test_that("convert_celsius_to_fahrenheit has argument named `temp` (visible)", {
  expect_equal(names(formals(convert_celsius_to_fahrenheit)), "temp")
})

test_that("convert_celsius_to_fahrenheit calculates correctly (visible)", {
  random_temps <- runif(10, -40, 100)
  c2f <- function(c) return(c * 9/5 + 32)
  expect_equal(convert_celsius_to_fahrenheit(random_temps[1]), 
               c2f(random_temps[1]))
  expect_equal(convert_celsius_to_fahrenheit(random_temps   ),
               c2f(random_temps   ))
})


# add_even

test_that("add_even has argument named `n` (visible)", {
  expect_equal(names(formals(add_even)), "n")
})

test_that("add_even calculates correctly (visible)", {
  random_n <- rpois(5, 20)
  my_add_even <- function(n) sum(seq(0, n, by = 2))
  for (i in 1:length(random_n)) {
    expect_equal(   add_even(random_n[i]), 
                 my_add_even(random_n[i]))
  }
})


# calculate_summary_statistics

test_that("calculate_summary_statistics has argument named `v` (visible)", {
  expect_equal(names(formals(calculate_summary_statistics)), "v")
})

test_that("calculate_summary_statistics output has correct names (visible)", {
  expect_setequal(names(calculate_summary_statistics(1:5)),
                  c("n", "mean", "sd", "lb", "ub"))
})

test_that("calculate_summary_statistics calculates correctly (visible)", {
  my_calculate_summary_statistics <- function(v) {
    l <- list(
      n    = length(v),
      mean = mean(v),
      sd   = sd(v)
    )
    l$lb = l$mean - 2 * l$sd / sqrt(l$n)
    l$ub = l$mean + 2 * l$sd / sqrt(l$n)
    
    return(l)
  }
  
  for (i in 1:5) {
    v <- rnorm(rpois(1, 10)+1)
    expect_equal(   calculate_summary_statistics(v),
                 my_calculate_summary_statistics(v))
  }
})


# summarize_data.frame_variable

test_that("summarize_data.frame_variable has arguments named `df` and `var` (visible)", {
  expect_equal(names(formals(summarize_data.frame_variable)), 
               c("df","var"))
})

test_that("summarize_data.frame_variable calculates correctly (visible)", {
  d <- data.frame(
    var1 = 1:10,
    var2 = runif(10),
    var3 = rnorm(10)
  )
  
  d$var1[1] <- NA
  d$var2[2] <- NA
  d$var3[3] <- NA
  
  expect_equal(summarize_data.frame_variable(d, "var1"), mean(d$var1, na.rm = TRUE))
  expect_equal(summarize_data.frame_variable(d, "var2"), mean(d$var2, na.rm = TRUE))
  expect_equal(summarize_data.frame_variable(d, "var3"), mean(d$var3, na.rm = TRUE))
})



test_that("get_regression_coefficients has arguments named `y` and `x` (visible)", {
  expect_equal(names(formals(get_regression_coefficients)),
               c("y","x"))
})


test_that("get_regression_coefficients returns intercept and slope (visible)", {
  n <- rpois(1, 20)
  y <- rnorm(n)
  x <- rnorm(n)

  expect_equal(get_regression_coefficients(y, x), coef(lm(y~x)))
})


