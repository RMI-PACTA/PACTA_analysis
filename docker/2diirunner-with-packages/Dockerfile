FROM maurolepore/2diidockerrunner:latest
MAINTAINER "Mauro Lepore" maurolepore@gmail.com

# This repository has binaries for Linux
RUN echo "options(repos = c(CRAN = 'https://packagemanager.rstudio.com/all/__linux__/bionic/latest'))" >> /root/.Rprofile \
    # update-dockerfile-packages-start
    && Rscript -e "install.packages( \
             c( \
               'suppressPackageStartupMessages({', \
               '  assertthat', \
               '  bookdown', \
               '  config', \
               '  conflicted', \
               '  countrycode', \
               '  data.table', \
               '  devtools', \
               '  dplyr', \
               '  fs', \
               '  fst', \
               '  ggplot2', \
               '  glue', \
               '  here', \
               '  janitor', \
               '  jsonlite', \
               '  knitr', \
               '  purrr', \
               '  readr', \
               '  readxl', \
               '  renv', \
               '  reshape2', \
               '  rlang', \
               '  rmarkdown', \
               '  rstudioapi', \
               '  scales', \
               '  stringr', \
               '  testthat', \
               '  tibble', \
               '  tidyr', \
               '  tidyselect', \
               '  usethis', \
               '  withr', \
               '  writexl', \
               '  zoo', \
               '})' \
             ) \
           )" \
    # update-dockerfile-packages-end
    && echo ""

