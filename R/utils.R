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

name_of_this_repo <- function() {
  ifelse(in_transitionmonitor(), "bound", "PACTA_analysis")
}

update_dockerfile_packages <- function(path = dockerfile_path()) {
  old_dockerfile <- read_dockerfile(path)
  new_dockerfile <- c(
    dockerfile_head(old_dockerfile),
    dockerfile_packages(packages_path()),
    dockerfile_tail(old_dockerfile)
  )
  writeLines(new_dockerfile, path)

  invisible(path)
}

read_dockerfile <- function(path = dockerfile_path()) {
  readLines(path, encoding = "UTF-8")
}

dockerfile_head <- function(dockerfile = read_dockerfile()) {
  dockerfile[1:end_of_packages_on_dockerfile(dockerfile)]
}

dockerfile_tail <- function(dockerfile = read_dockerfile()) {
  dockerfile[start_of_packages_on_dockerfile(dockerfile):length(dockerfile)]
}

dockerfile_packages <- function(path = packages_path()) {
  raw <- readLines(path, encoding = "UTF-8")
  pkg <- sub("library\\((.*)\\)", "\\1", raw)

  c(
    '    && Rscript -e "install.packages( \\',
    paste0('             ', format_as_vector(pkg), ' \\'),
    '           )" \\'
  )
}

format_as_vector <- function(string) {
  x <- glue("'{string}',")
  x[length(x)] <- sub(",$", "", x[length(x)])
  c('c(', glue("  {x}"), ')' )
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
