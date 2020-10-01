test_integration <- function (input = "integration-test.Rmd") {
  rmarkdown::render(input)
}

setup_project <- function() {
  source(file.path(
      "dry", "set_portfolio-name-ref-all_working-location_and_web-parameters.R"
  ))
}
