# Help run an integration test, hiding its implementation details
test_integration <- function (input = "integration-test.Rmd") {
  rmarkdown::render(input)
}
