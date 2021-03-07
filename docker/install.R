# 2020-10-06 (see https://packagemanager.rstudio.com/client/#/repos/1/overview)
snapshot <- "https://packagemanager.rstudio.com/all/__linux__/focal/342"

# Build an image with R packages from a specific date `snapshot`:
# docker build \
#     --build-arg BUILD_DATE=yyyy-mm-dd \
#     -t 2dii/pacta_analysis:yyyy-mm-dd .

options(repos = c(CRAN = snapshot))
install.packages(
  c(
    'assertthat',
    'bookdown',
    'config',
    'conflicted',
    'countrycode',
    'crayon',
    'data.table',
    'devtools',
    'dplyr',
    'forcats',
    'fs',
    'fst',
    'ggplot2',
    'glue',
    'highcharter',
    'here',
    'janitor',
    'jsonlite',
    'knitr',
    'purrr',
    'readr',
    'readxl',
    'renv',
    'reshape2',
    'rlang',
    'rmarkdown',
    'rstudioapi',
    'scales',
    'stringr',
    'testthat',
    'tibble',
    'tidyr',
    'tidyselect',
    'usethis',
    'withr',
    'writexl',
    'zoo'
  )
)
