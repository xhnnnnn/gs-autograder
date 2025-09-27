# grade.R — HW6 minimal runner (Gradescope/Docker compatible)

suppressPackageStartupMessages(library(gradeR))

# Always run inside the submission directory
setwd("/autograder/submission")

# Student script name must match the assignment spec
student_in  <- "HW06_TidyPart02.R"
tests_path  <- "/autograder/source/tests.R"
results_dst <- "/autograder/results/results.json"

stopifnot(file.exists(student_in), file.exists(tests_path))

# Expose the absolute path of the student script to tests.R (for string checks)
Sys.setenv(GS_STUDENT_FILE = normalizePath(student_in, winslash = "/", mustWork = TRUE))

# Run the test suite; gradeR will generate results.json
calcGradesForGradescope(student_in, tests_path)

# Fallback copy if gradeR wrote results.json to CWD instead of /autograder/results
if (!file.exists(results_dst) && file.exists("results.json")) {
  file.copy("results.json", results_dst, overwrite = TRUE)
}
