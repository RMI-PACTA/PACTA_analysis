setwd(here::here())
function_name <- "get_and_clean_company_fin_data"
result <- get_and_clean_company_fin_data("tests/clean_data_tests/example_data")

test_that(paste0(function_name, "() function exists"), {
  expect_true(
    exists(function_name)
  )
})

test_that(paste0(function_name, "() returns a data.frame"), {
  expect_true(
    inherits(result, "data.frame")
  )
})
