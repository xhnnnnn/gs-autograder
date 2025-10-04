suppressPackageStartupMessages(library(gradeR))

student_file <- "HW07_TidyPart03.R"
tests_file   <- "/autograder/source/tests.R"

calcGradesForGradescope(
  file.path("/autograder/submission", student_file),
  tests_file
)
