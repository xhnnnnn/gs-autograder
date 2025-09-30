# Gradescope R Autograder (HW6, Docker Template)

This repository contains the Docker-based Gradescope autograder for **HW6** (STAT 5279).

## Directory Structure

```
.
├─ Dockerfile              # Docker image definition
├─ run_autograder          # Entry script; Gradescope calls this
├─ source/                 
│  ├─ grade.R              # Main grading script
│  ├─ tests.R              # testthat tests for HW6
│  ├─ answers.RDS          # Reference answers for grading
├─ local_submit/           # Local folder to simulate a student's submission (not uploaded)
├─ results/                # Output folder; contains results.json after grading
├─ .gitattributes          
├─ .gitignore              
└─ README.md               
```

## Build the Docker Image

```bash
docker build -t gs-r-autograder-hw6:dev .
```

## Run Locally

Put a student submission (e.g. `HW06_TidyPart02.R`) in `local_submit/`, then run:

```bash
docker run --rm \
  -v "$PWD/local_submit:/autograder/submission" \
  -v "$PWD/results:/autograder/results" \
  -v "$PWD/source:/autograder/source" \
  gs-r-autograder-hw6:dev /autograder/run_autograder
```

The grading results will be written to:

```
results/results.json
```

## Notes

* Uses **testthat** + **gradeR** for grading.
* Only `answers.RDS` is required for HW6 (no external `.RData` file).
* Edit `tests.R` for each assignment; `grade.R` should usually not need changes.
