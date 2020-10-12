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
  raw <- readLines(packages_path(), encoding = "UTF-8")
  pkg <- sub("library\\((.*)\\)", "\\1", raw)
  pkg <- glue('    && Rscript -e "install.packages(\'{pkg}\')" \\')

  dkr <- readLines(path, encoding = "UTF-8")
  dkr_updated <- c(
    dkr[1:end_of_packages_on_dockerfile(dkr)],
    pkg,
    dkr[start_of_packages_on_dockerfile(dkr):length(dkr)]
  )
  writeLines(dkr_updated, path)

  invisible(path)
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
