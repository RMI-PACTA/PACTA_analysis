test_integration <- function (input = "integration-test.Rmd") {
  needless <- tempfile(fileext = ".html")
  rmarkdown::render(input, output_file = needless)
}

setup_project <- function() {
  source(path(
      "dry", "set_portfolio-name-ref-all_working-location_and_web-parameters.R"
  ))
}
