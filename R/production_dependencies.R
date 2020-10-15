production_dependencies <- function(path = packages_path()) {
  sub("library\\((.*)\\)", "\\1", readLines(path))
}

#' @examples
#' detected_dependencies()
#' detected_dependencies(exclude = not_for_production())
#' @noRd
detected_dependencies <- function(exclude = NULL) {
  deps <- find_dependencies()

  if (!is.null(exclude)) {
    is_for_production <- !grepl(exclude, deps$source)
    deps <- deps[is_for_production, ]
  }

  sort(unique(deps$package))
}

not_for_production <- function() {
  patterns <- c(
    "0_graphing_functions.R",
    "renvignore",
    "data-raw",
    "integration-test",
    "DESCRIPTION",
    "R/",
    "tests/"
  )

  paste(patterns, collapse = "|")
}

find_dependencies <- function(tibble, as_tibble, renv, dependencies) {
  dependencies <- dplyr::bind_rows(
    dependencies_in("PACTA_analysis"),
    dependencies_in("create_interactive_report"),
    dependencies_in("StressTestingModelDev")
  )

  tibble::as_tibble(rlang::set_names(dependencies, tolower))
}

dependencies_in <- function(project_name) {
  path <- fs::path(fs::path_dir(here::here()), project_name)
  renv::dependencies(path, progress = FALSE)
}
