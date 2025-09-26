# grade.R

suppressPackageStartupMessages(library(gradeR))

# Always work in submission so tests can read answers.RDS etc.
setwd("/autograder/submission")

student_in  <- "HW05_TidyPart01.R"
tests_path  <- "/autograder/source/tests.R"
results_dst <- "/autograder/results/results.json"

stopifnot(file.exists(student_in), file.exists(tests_path))

# Let tests.R read the student's raw source text (absolute path)
Sys.setenv(GS_STUDENT_FILE = normalizePath(student_in, winslash = "/", mustWork = FALSE))

# If hidden data exists, run it in the same environment as the student's code/tests:
tmp <- "HW05_TidyPart01.withdata.R"
hdr <- if (file.exists("HW05data.RData")) "load('HW05data.RData')" else NULL
writeLines(c(hdr, readLines(student_in, warn = FALSE)), tmp)

# Run gradeR
calcGradesForGradescope(tmp, tests_path)

# Minimal post-step:
# If gradeR already wrote to /autograder/results, do nothing.
# Else if results.json is in CWD, copy it once.
if (!file.exists(results_dst) && file.exists("results.json")) {
  file.copy("results.json", results_dst, overwrite = TRUE)
}
