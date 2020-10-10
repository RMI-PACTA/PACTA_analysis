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
}

packages_path <- function() {
  "deduplicate/load-and-attach-r-packages.R"
}

create_install_calls_for_dockerfile <- function(path = packages_path()) {
  pkg <- sub("library\\((.*)\\)", "\\1", readLines(path, encoding = "UTF-8"))
  glue('    && Rscript -e "install.packages(\'{pkg}\')" \\')
}

update_dockerfile_packages <- function(path = "docker/Dockerfile") {
  dkr <- readLines(path, encoding = "UTF-8")

  raw <- readLines(packages_path(), encoding = "UTF-8")
  pkg <- sub("library\\((.*)\\)", "\\1", raw)
  pkg <- glue::glue('    && Rscript -e "install.packages(\'{pkg}\')" \\')

  out <- c(
    dkr[1:marker_start(dkr)],
    pkg,
    dkr[marker_end(dkr):length(dkr)]
  )
  writeLines(out, path)

  invisible(path)
}

marker_start <- function(lines) {
  grep("# update-dockerfile-packages-start", lines)
}

marker_end <- function(lines) {
  grep("# update-dockerfile-packages-end", lines)
}

