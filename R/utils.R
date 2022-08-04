setup_project <- function() {
  if (in_transitionmonitor()) {
    portfolio_name_ref_all <<- get_portfolio_name()
    working_location <<- getwd()
    set_web_parameters(file_path = paste0(working_location, "/parameter_files/WebParameters_docker.yml"))
  } else {
    portfolio_name_ref_all <<- portfolio_name_ref_all # must be the same name as in the _PortfolioParameters.yml
    working_location <<- here::here()
    set_web_parameters(file_path = paste0(working_location, "/parameter_files/WebParameters_2dii.yml"))
  }
}

required_packages_vec <- function() {
  c(
    "bookdown",
    "devtools",
    "dplyr",
    "fs",
    "fst",
    "ggplot2",
    "glue",
    "here",
    "janitor",
    "jsonlite",
    "knitr",
    "purrr",
    "readr",
    "readxl",
    "rlang",
    "rmarkdown",
    "scales",
    "stringr",
    "tibble",
    "tidyr",
    "tidyselect",
    "writexl",
    "zoo"
  )
}

use_r_packages <- function() {
  suppressPackageStartupMessages({
    for (pkg in required_packages_vec()) {
      library(pkg, character.only = TRUE)
    }
  })
}

expect_no_message <- function(object, regexp = NA, ...) {
  testthat::expect_message(object = object, regexp = regexp, ...)
}

get_build_version <- function(env_var = "build_version") {
  build_version <- Sys.getenv(env_var)
  ifelse(nzchar(build_version), paste0("v", build_version), NA_character_)
}

get_build_version_msg <- function() {
  build_version <- get_build_version()
  ifelse(is.na(build_version), "", paste0(" (Docker build ", build_version, ")"))
}
