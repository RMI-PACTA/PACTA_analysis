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

#' Style .R and .Rmd files
#'
#' To style only R-package sources useuse_tidy_style()
#'
#' @examples
#' style_all()
#' @noRd
style_all <- function() {
  style_pattern("[.]R$")
  style_pattern("[.]Rmd$")
}

style_pattern <- function(pattern) {
  files <- list.files(pattern = pattern, recursive = TRUE)
  purrr::walk(files, styler::style_file)
}

