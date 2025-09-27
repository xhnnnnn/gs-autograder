FROM gradescope/autograder-base:ubuntu-22.04

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      r-base r-base-dev libcurl4-openssl-dev libssl-dev libxml2-dev && \
    rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN R -e 'install.packages(c("remotes"), repos="https://cloud.r-project.org")' && \
    R -e 'install.packages(c("gradeR","testthat","tidyverse","ggplot2","dplyr","rlang"), repos="https://cloud.r-project.org")'


WORKDIR /autograder
COPY source /autograder/source
COPY run_autograder /autograder/run_autograder
RUN chmod +x /autograder/run_autograder && chmod -R a+rX /autograder
