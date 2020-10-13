FROM 2dii/system
MAINTAINER "Mauro Lepore" maurolepore@gmail.com

# This repository has binaries for Linux
RUN echo 'options(repos = c(CRAN = "https://packagemanager.rstudio.com/all/__linux__/bionic/latest"))' >> ~/.Rprofile \
  && echo 'options(HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(), paste(getRversion(), R.version$platform, R.version$arch, R.version$os)))' >> ~/.Rprofile \
    # update-dockerfile-packages-start
    && Rscript -e "install.packages( \
             c( \
               'assertthat', \
               'bookdown', \
               'config', \
               'conflicted', \
               'countrycode', \
               'data.table', \
               'devtools', \
               'dplyr', \
               'fs', \
               'fst', \
               'ggplot2', \
               'glue', \
               'here', \
               'janitor', \
               'jsonlite', \
               'knitr', \
               'purrr', \
               'readr', \
               'readxl', \
               'renv', \
               'reshape2', \
               'rlang', \
               'rmarkdown', \
               'rstudioapi', \
               'scales', \
               'stringr', \
               'testthat', \
               'tibble', \
               'tidyr', \
               'tidyselect', \
               'usethis', \
               'withr', \
               'writexl', \
               'zoo' \
             ) \
           )" \
    # update-dockerfile-packages-end
    && echo ""

