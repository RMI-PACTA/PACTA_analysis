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

