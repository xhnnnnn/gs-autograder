FROM gradescope/autograder-base:ubuntu-22.04

# --- Install system dependencies ---
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      libxml2-dev libcurl4-openssl-dev libssl-dev \
      libfontconfig1-dev \
      libharfbuzz-dev libfribidi-dev \
      libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev libwebp-dev \
      r-base r-base-dev && \
    rm -rf /var/lib/apt/lists/*


# --- Install R packages ---
RUN Rscript -e "install.packages(c('gradeR','testthat'))"

WORKDIR /autograder

# Copy grading code
COPY source /autograder/source
COPY run_autograder /autograder/run_autograder

# Permissions
RUN chmod +x /autograder/run_autograder && chmod -R a+rX /autograder
