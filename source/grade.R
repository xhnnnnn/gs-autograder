suppressPackageStartupMessages(library(gradeR))
suppressPackageStartupMessages(library(jsonlite))  # minimal extra dep for JSON

student_file <- "HW07_TidyPart03.R"
tests_file   <- "/autograder/source/tests.R"

write_fail_json <- function(msg) {
  payload <- list(
    visibility = "visible",
    tests = list(list(
      name = "Autograder crashed (R error)",
      score = 0, max_score = 1,
      output = msg
    ))
  )
  writeLines(toJSON(payload, auto_unbox = TRUE, pretty = TRUE),
             "/autograder/results/results.json")
}

tryCatch(
  {
    calcGradesForGradescope(
      file.path("/autograder/submission", student_file),
      tests_file
    )
  },
  error = function(e) write_fail_json(conditionMessage(e))
)
