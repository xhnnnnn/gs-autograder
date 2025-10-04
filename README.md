
This repository contains the Docker-based Gradescope autograder for **HW7** (STAT 5279).

 ## Directory Structure

 ```
 .
 ├─ Dockerfile              # Docker image definition
 ├─ run_autograder          # Entry script; Gradescope calls this
 ├─ source/                 
 │  ├─ grade.R              # Main grading script
-│  ├─ tests.R              # testthat tests for HW6
+│  ├─ tests.R              # testthat tests for HW7
 │  ├─ answers.RDS          # Reference answers for grading
+│  ├─ blocks.RData         # Supporting data files used by the tests
+│  ├─ rex.csv
+│  └─ shuttle.RDS
 ├─ local_submit/           # Local folder to simulate a student's submission (not uploaded)
 ├─ results/                # Output folder; contains results.json after grading
 ├─ .gitattributes          
 ├─ .gitignore              
 └─ README.md               
 ```

 ## Build the Docker Image

 ```bash
docker build -t gs-r-autograder-hw7:dev .
 ```

 ## Run Locally

+Put a student submission (e.g. `HW07_TidyPart03.R`) in `local_submit/`, then run:

 ```bash
 docker run --rm \
   -v "$PWD/local_submit:/autograder/submission" \
   -v "$PWD/results:/autograder/results" \
   -v "$PWD/source:/autograder/source" \
- gs-r-autograder-hw7:dev /autograder/run_autograder
 ```

 The grading results will be written to:

 ```
 results/results.json
 ```

 ## Notes

 * Uses **testthat** + **gradeR** for grading.
-* Only `answers.RDS` is required for HW6 (no external `.RData` file).
+* Student submissions must be named `HW07_TidyPart03.R`.
+* All supporting datasets (`answers.RDS`, `blocks.RData`, `rex.csv`, `shuttle.RDS`) are bundled in `source/` and copied into the autograder image.
 * Edit `tests.R` for each assignment; `grade.R` should usually not need changes.
