#' Write calls to library(package) for packages detected in this project
#'
#' @examples
#' tmp <- tempfile()
#' update_r_packages(tmp)
#'
#' writeLines(readLines(tmp))
#' @noRd
update_r_packages <- function(path = r_packages_path(),
                              projects = pacta_projects()) {
  packages <- r_packages(projects = projects)
  library_calls <- sprintf("library(%s)", packages)

  writeLines(library_calls, path)

  invisible(path)
}

#' Path where both the source and package can find it
#' @examples
#' r_packages_path()
#' @noRd
r_packages_path <- function() {
  here::here("inst", "extdata", "r_packages.R")
}

#' @examples
#' dir_ls(extdata_path())
#' extdata_path()
#' extdata_path(fs::path_file(r_packages_path()))
#' @noRd
extdata_path <- function(...) {
  system.file("extdata", ..., package = "PACTA.analysis", mustWork = TRUE)
}

r_packages <- function(projects = pacta_projects()) {
  packages <- find_dependencies(projects)$package
  # Not sure why renv detects R itself as a dependency
  packages <- exclude(packages, "^R$")
  sort(unique(packages))
}

exclude <- function(x, pattern) {
  grep(pattern, x, value = TRUE, invert = TRUE)
}
