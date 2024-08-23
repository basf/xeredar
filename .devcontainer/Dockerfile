FROM r-base:4.3.2

ARG PROJECT_NAME
EXPOSE 3838

ARG R_REMOTES_NO_ERRORS_FROM_WARNINGS=false

RUN apt-get update \
    && apt-get install -y cmake

RUN wget https://github.com/jgm/pandoc/releases/download/2.11.4/pandoc-2.11.4-1-amd64.deb \
  && dpkg -i pandoc-2.11.4-1-amd64.deb \
  && rm pandoc-2.11.4-1-amd64.deb


RUN Rscript -e 'install.packages("remotes");remotes::install_cran(c("ARTool", "broom@1.0.5", "bslib@0.6.1", "car@3.1.2", "dplyr@1.1.3", "emmeans@1.10.0", "fBasics@4032.96", "ggplot2@3.4.4", "gt@0.10.0", "lme4@1.1.35.5", "magrittr@2.0.3", "MASS@7.3.60", "methods@4.3.2", "multcomp@1.4.25", "parameters@0.21.3", "performance@0.10.8","rlang@1.1.2", "shiny@1.8.0","shinydashboard@0.7.2","shinyWidgets@0.8.1", "tidyr@1.3.0"))'

USER 999:999
