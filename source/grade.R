# grade.R — Preflight student's script (same working directory as grading),
# surface precise runtime errors to results.json, then run gradeR tests only if preflight passes.

suppressPackageStartupMessages(library(gradeR))
suppressPackageStartupMessages(library(jsonlite))

RESULTS_JSON <- "/autograder/results/results.json"
STUDENT_FILE <- "HW08_ProgrammingPart01.R"
TESTS_FILE   <- "/autograder/source/tests.R"
SUB_FILE     <- file.path("/autograder/submission", STUDENT_FILE)  # student script path
SRC_DIR      <- "/autograder/source"                                # grading working directory

write_fail_json <- function(stderr_tail, stdout_tail = "") {
  # Produce a Gradescope-compatible JSON with a clear, student-facing error report.
  payload <- list(
    visibility = "visible",
    tests = list(list(
      name = "Script execution error",
      score = 0,
      max_score = 1,
      output = paste(
        "**Autograder could not execute your code.**",
        "This happened because your R script encountered an error during execution.",
        if (nzchar(stderr_tail)) paste0("\n\n**Error output (last lines):**\n", stderr_tail) else "",
        if (nzchar(stdout_tail)) paste0("\n\n**Program output before the crash (last lines):**\n", stdout_tail) else "",
        sep = "\n"
      )
    ))
  )
  writeLines(jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE),
             RESULTS_JSON, useBytes = TRUE)
}

# --- 1) Preflight: run the student's script EXACTLY as written in the same WD as grading.
if (!file.exists(SUB_FILE)) {
  write_fail_json(sprintf("Missing submission file at %s.", SUB_FILE))
  quit(save = "no")
}

pre_stdout <- tempfile("preflight_stdout_", fileext = ".log")
pre_stderr <- tempfile("preflight_stderr_", fileext = ".log")

old_wd <- getwd()
setwd(SRC_DIR)  # match the grading working directory used by your run_autograder
status <- tryCatch(
  system2("Rscript",
          c("--vanilla", SUB_FILE),
          stdout = pre_stdout, stderr = pre_stderr, wait = TRUE),
  error = function(e) 999L  # failed to launch Rscript
)
setwd(old_wd)

if (!identical(status, 0L)) {
  # Collect tails of stderr/stdout for a concise, actionable error message.
  stderr_tail <- tryCatch(paste(utils::tail(readLines(pre_stderr, warn = FALSE), 80), collapse = "\n"),
                          error = function(...) "")
  stdout_tail <- tryCatch(paste(utils::tail(readLines(pre_stdout, warn = FALSE), 40), collapse = "\n"),
                          error = function(...) "")
  if (!nzchar(stderr_tail) && status == 999L) {
    stderr_tail <- "Failed to invoke Rscript (runtime not available in the image)."
  }
  write_fail_json(stderr_tail, stdout_tail)
  quit(save = "no")  # stop here; do NOT run tests on a broken script
}

# --- 2) If preflight passed (script ran cleanly), run your original gradeR tests.
calcGradesForGradescope(SUB_FILE, TESTS_FILE)
