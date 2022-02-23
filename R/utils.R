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

#' @examples
#' some_dockerfile <- tempfile()
#' writeLines(create_empty_dockerfile("library(dplyr)"), some_dockerfile)
#'
#' # Before
#' writeLines(readLines(some_dockerfile))
#' update_dockerfile_packages(path = some_dockerfile)
#' # After
#' writeLines(readLines(some_dockerfile))
#' @noRd
update_dockerfile_packages <- function(path = NULL) {
  path <- path %||% path_to_empty_dockerfile()

  old_dockerfile <- read_dockerfile(path)
  new_dockerfile <- c(
    dockerfile_head(old_dockerfile),
    dockerfile_packages(),
    dockerfile_tail(old_dockerfile)
  )

  writeLines(new_dockerfile, path)

  invisible(path)
}

path_to_empty_dockerfile <- function() {
  tmp <- tempfile()
  writeLines(create_empty_dockerfile(), tmp)
  tmp
}

create_empty_dockerfile <- function(x = "") {
  c(mark_start(), x, mark_end())
}

read_dockerfile <- function(path = dockerfile_path()) {
  readLines(path, encoding = "UTF-8")
}

dockerfile_head <- function(dockerfile = read_dockerfile()) {
  dockerfile[1:start_of_packages_on_dockerfile(dockerfile)]
}

dockerfile_tail <- function(dockerfile = read_dockerfile()) {
  dockerfile[end_of_packages_on_dockerfile(dockerfile):length(dockerfile)]
}

# styler: off
dockerfile_packages <- function() {
  pkg <- required_packages_vec()

  c(
    '    && Rscript -e "install.packages( \\',
    paste0("             ", format_as_vector(pkg), " \\"),
    '           )" \\'
  )
}
# styler: on

# styler: off
format_as_vector <- function(string) {
  x <- glue("'{string}',")
  x[length(x)] <- sub(",$", "", x[length(x)])
  c("c(", glue("  {x}"), ")")
}
# styler: on

dockerfile_path <- function() {
  file.path("docker", "2diirunner-with-packages", "Dockerfile")
}

end_of_packages_on_dockerfile <- function(lines) {
  grep(mark_end(), lines)
}

start_of_packages_on_dockerfile <- function(lines) {
  grep(mark_start(), lines)
}

mark_start <- function() {
  "# update-dockerfile-packages-start"
}

mark_end <- function() {
  "# update-dockerfile-packages-end"
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
