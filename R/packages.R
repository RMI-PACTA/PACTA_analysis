#' Name of the packages we use in production
#'
#' Developer: After you add or remove packages from this vector, you may run:
#' * update_dockerfile_packages()
#' * update_production_packages()
#'
#' @return An alpha-sorted character-vector.
#' @examples
#' # Common workflow:
#'
#' # 1. See what packages we use
#' packages()
#'
#' #' # 2. Manualy add/remove packages from the body of packages()
#'
#' #' # 3. Update related files
#' update_production_packages()
#' update_dockerfile_packages()
#' @noRd
packages <- function() {
  sort(c(
    "assertthat",
    "bookdown",
    "config",
    "conflicted",
    "countrycode",
    "data.table",
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
    "renv",
    "reshape2",
    "rlang",
    "rstudioapi",
    "scales",
    "stringr",
    "testthat",
    "tibble",
    "tidyr",
    "tidyselect",
    "usethis",
    "withr",
    "writexl",
    "zoo"
  ))
}

use_r_packages <- function(path = packages_path()) {
  suppressPackageStartupMessages(source(path))
  source(file.path("deduplicate", "resolve_conflicts.R"))
  invisible(path)
}

packages_path <- function() {
  here::here("deduplicate", "production_packages.R")
}

#' @examples
#' tmp <- tempfile()
#' update_production_packages(tmp)
#'
#' # See result
#' writeLines(readLines(tmp))
#' @noRd
update_production_packages <- function(path = packages_path()) {
  library_package <- sprintf("library(%s)", packages())
  writeLines(library_package, path)

  invisible(packages_path())
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
#'
#' # By defaul it writes to a temporary file
#' tmp <- update_dockerfile_packages()
#' writeLines(readLines(tmp))
#' @noRd
update_dockerfile_packages <- function(path = NULL) {
  path <- path %||% path_to_empty_dockerfile()

  old_dockerfile <- readLines(path, encoding = "UTF-8")
  new_dockerfile <- c(
    dockerfile_head(old_dockerfile),
    dockerfile_packages(packages_path()),
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

dockerfile_head <- function(lines) {
  lines[1:start_of_packages_on_dockerfile(lines)]
}

dockerfile_tail <- function(lines) {
  lines[end_of_packages_on_dockerfile(lines):length(lines)]
}

dockerfile_packages <- function(path = packages_path()) {
  c(
    '    && Rscript -e "install.packages( \\',
    paste0("             ", format_as_vector(packages()), " \\"),
    '           )" \\'
  )
}

format_as_vector <- function(string) {
  x <- glue("'{string}',")
  x[length(x)] <- sub(",$", "", x[length(x)])
  c("c(", glue("  {x}"), ")")
}

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
