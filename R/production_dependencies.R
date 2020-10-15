production_dependencies <- function(path = packages_path()) {
  sub("library\\((.*)\\)", "\\1", readLines(path))
}

#' @examples
#' detected_dependencies()
#' detected_dependencies(exclude = not_for_production())
#' @noRd
detected_dependencies <- function(exclude = NULL) {
  deps <- dependencies_df()

  if (!is.null(exclude)) {
    is_for_production <- !grepl(exclude, deps$source)
    deps <- deps[is_for_production, ]
  }

  sort(unique(deps$package))
}

dependencies_df <- function(tibble, as_tibble, renv, dependencies) {
  renv::dependencies(here::here(), progress = FALSE) %>%
    rlang::set_names(tolower) %>%
    tibble::as_tibble()
}

detect_production_path <- function(path, exclude) {
  !grepl(exclude, path)
}

not_for_production <- function() {
  patterns <- c(
    "data-raw",
    "deduplicate/load-and-attach-r-packages.R",
    "deduplicate/production_packages.R",
    "DESCRIPTION",
    "integration-test",
    "R/",
    "tests/"
  )

  paste(patterns, collapse = "|")
}

