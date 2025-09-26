# Gradescope R Autograder (Docker Template)

This repository contains a minimal example of a Gradescope autograder for R assignments, packaged with Docker.

## Directory Structure
```

.
├─ Dockerfile              # Docker image 
├─ run_autograder          # Entry script; Gradescope calls this
├─ source/                 
│  ├─ grade.R              # Main grading script; 
│  ├─ tests.R              # testthat tests 
│  ├─ answers.RDS          
│  └─ HW05data.RData       # if needed 
├─ local_submit/           # Local folder to simulate a student's submission (not uploaded)
├─ results/                # Output folder; must contain results.json after grading
├─ .gitattributes          
├─ .gitignore              
└─ README.md               


````

## Build the Docker Image
```bash
docker build -t gs-r-autograder:dev .
````

## Run Locally

Put a student submission in `local_submit/`, then run:

```bash
docker run --rm \
  -v "$PWD/local_submit:/autograder/submission" \
  -v "$PWD/results:/autograder/results" \
  -v "$PWD/source:/autograder/source" \
  gs-r-autograder:dev /autograder/run_autograder
```

Results will be written to:

```
results/results.json
```

## Notes

* Uses `testthat` + `gradeR` for grading.
* `answers.RDS` and any `.RData` are copied into the submission directory automatically.
* Edit `tests.R` for each assignment; `grade.R` should usually not need changes.
