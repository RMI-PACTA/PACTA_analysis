test_integration <- function(input = "integration-test.Rmd") {
  needless <- tempfile(fileext = ".html")
  rmarkdown::render(input, output_file = needless)

  invisible(input)
}

setup_project <- function() {
  path <- fs::path(
    "deduplicate",
    "set_portfolio-name-ref-all_working-location_and_web-parameters.R"
  )
  source(path)
}

use_r_packages <- function(path = packages_path()) {
  source(path)
  conflicted::conflict_prefer("filter", "dplyr")
  conflicted::conflict_prefer("lag", "dplyr")

  invisible(path)
}

this_repo <- function() {
  ifelse(in_transitionmonitor(), "bound", "PACTA_analysis")
}

update_dockerfile_packages <- function(path = dockerfile_path()) {
  dkr <- readLines(path, encoding = "UTF-8")

  updated_dockerfile <- c(
    dockerfile_header(dkr),
    dockerfile_packages(packages_path()),
    dockerfile_footer(dkr)
  )
  writeLines(updated_dockerfile, path)

  invisible(path)
}

dockerfile_header <- function(dkr) {
  dkr[1:end_of_packages_on_dockerfile(dkr)]
}

dockerfile_footer <- function(dkr) {
  dkr[start_of_packages_on_dockerfile(dkr):length(dkr)]
}

dockerfile_packages <- function() {
  raw <- readLines(path, encoding = "UTF-8")
  pkg <- sub("library\\((.*)\\)", "\\1", raw)
  glue('    && Rscript -e "install.packages(\'{pkg}\')" \\')
}

packages_path <- function() {
  "deduplicate/load-and-attach-r-packages.R"
}

dockerfile_path <- function() {
  "docker/2diirunner-with-packages/Dockerfile"
}

start_of_packages_on_dockerfile <- function(lines) {
  grep("# update-dockerfile-packages-end", lines)
}

end_of_packages_on_dockerfile <- function(lines) {
  grep("# update-dockerfile-packages-start", lines)
}
