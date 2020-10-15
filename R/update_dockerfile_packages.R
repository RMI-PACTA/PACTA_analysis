#' @examples
#' some_dockerfile <- tempfile()
#' writeLines(create_empty_dockerfile("library(dplyr)"), some_dockerfile)
#'
#' # Before
#' writeLines(readLines(some_dockerfile))
#' update_dockerfile_packages(path = some_dockerfile)
#' # After
#' writeLines(readLines(some_dockerfile))
#'
#' # By defaul it writes to a temporary file
#' tmp <- update_dockerfile_packages()
#' writeLines(readLines(tmp))
#' @noRd
update_dockerfile_packages <- function(dockerfile = NULL,
                                       r_packages = r_packages_path()) {
  dockerfile <- dockerfile %||% path_to_empty_dockerfile()

  old_dockerfile <- readLines(dockerfile, encoding = "UTF-8")
  new_dockerfile <- c(
    dockerfile_head(old_dockerfile),
    dockerfile_packages(r_packages),
    dockerfile_tail(old_dockerfile)
  )

  writeLines(new_dockerfile, dockerfile)

  invisible(dockerfile)
}

path_to_empty_dockerfile <- function() {
  path <- tempfile()
  writeLines(create_empty_dockerfile(contents = ""), path)
  path
}

create_empty_dockerfile <- function(contents = "") {
  c(mark_start(), contents, mark_end())
}

dockerfile_head <- function(lines) {
  lines[1:start_of_packages_on_dockerfile(lines)]
}

dockerfile_tail <- function(lines) {
  lines[end_of_packages_on_dockerfile(lines):length(lines)]
}

dockerfile_packages <- function(path = r_packages_path()) {
  c(
    '    && Rscript -e "install.packages( \\',
    paste0("             ", format_as_vector(readLines(path)), " \\"),
    '           )" \\'
  )
}

format_as_vector <- function(string) {
  x <- glue("'{string}',")
  x[length(x)] <- sub(",$", "", x[length(x)])
  c("c(", glue("  {x}"), ")")
}

dockerfile_path <- function() {
  path("docker", "2diirunner-with-packages", "Dockerfile")
}

end_of_packages_on_dockerfile <- function(dockerfile) {
  grep(mark_end(), dockerfile)
}

start_of_packages_on_dockerfile <- function(dockerfile) {
  grep(mark_start(), dockerfile)
}

mark_start <- function() {
  "# update-dockerfile-packages-start"
}

mark_end <- function() {
  "# update-dockerfile-packages-end"
}

