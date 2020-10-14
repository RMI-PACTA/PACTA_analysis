#' Name of the packages we use in production
#'
#' @return An alpha-sorted character-vector.
#' @examples
#' packages()
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
    "rmarkdown",
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
