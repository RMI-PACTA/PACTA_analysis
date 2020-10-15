production_dependencies <- function() {
  find_dependencies() %>%
    dplyr::filter(detect_production_path(source)) %>%
    dplyr::mutate(source = remove_path_to_here(.data$source)) %>%
    dplyr::select(.data$source, .data$package) %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$package)
}

find_dependencies <- function(tibble, as_tibble, renv, dependencies) {
  renv::dependencies(progress = FALSE) %>%
    rlang::set_names(tolower) %>%
    tibble::as_tibble()
}

detect_production_path <- function(path) {
  !grepl(patterns_to_exclude(), path)
}

patterns_to_exclude <- function() {
  paste(exclude_from_production(), collapse = "|")
}

exclude_from_production <- function() {
  c(
    "data-raw",
    "deduplicate/load-and-attach-r-packages.R",
    "deduplicate/production_packages.R",
    "DESCRIPTION",
    "integration-test",
    "R/",
    "tests/"
  )
}

remove_path_to_here <- function(x) {
  sub(paste0(here::here(), "/"), "", x)
}
