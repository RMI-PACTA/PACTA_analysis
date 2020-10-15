setup_project <- function() {
  path <- fs::path(
    "deduplicate",
    "set_portfolio-name-ref-all_working-location_and_web-parameters.R"
  )
  source(path)
}

use_r_packages <- function(path = packages_path()) {
  suppressPackageStartupMessages(source(path))
  resolve_conflicts()

  invisible(path)
}

resolve_conflicts <- function() {
  conflicted::conflict_prefer("filter", "dplyr")
  conflicted::conflict_prefer("lag", "dplyr")
  conflicted::conflict_prefer("mutate", "dplyr")
  conflicted::conflict_prefer("here", "here")
  conflicted::conflict_prefer("rename", "dplyr")
  conflicted::conflict_prefer("summarise", "dplyr")
  conflicted::conflict_prefer("arrange", "dplyr")
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
#' @noRd
update_dockerfile_packages <- function(path = NULL) {
  path <- path %||% path_to_empty_dockerfile()

  old_dockerfile <- read_dockerfile(path)
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

read_dockerfile <- function(path = dockerfile_path()) {
  readLines(path, encoding = "UTF-8")
}

dockerfile_head <- function(dockerfile = read_dockerfile()) {
  dockerfile[1:start_of_packages_on_dockerfile(dockerfile)]
}

dockerfile_tail <- function(dockerfile = read_dockerfile()) {
  dockerfile[end_of_packages_on_dockerfile(dockerfile):length(dockerfile)]
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
  file.path("deduplicate", "load-and-attach-r-packages.R")
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
