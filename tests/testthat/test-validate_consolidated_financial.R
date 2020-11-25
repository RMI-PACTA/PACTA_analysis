test_that("validate_consolidated_financial() function exists", {
  expect_true(
    class(validate_consolidated_financial) == "function"
  )
})

test_that("validate_consolidated_financial() uses a function name that conforms to the list of data_object_names", {
  expect_true(
    sub("^validate_", "", deparse(substitute(validate_consolidated_financial))) %in% data_object_names
  )
})

test_that("validate_consolidated_financial() returns TRUE for example data", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/consolidated_financial_data.rds"))
  expect_true(
    validate_consolidated_financial(example_data)
  )
})

test_that("validate_consolidated_financial() returns FALSE for data with no columns", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/consolidated_financial_data.rds"))
  expect_false(
    validate_consolidated_financial(data.frame())
  )
})

test_that("validate_consolidated_financial() returns FALSE for data with a specified column missing", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/consolidated_financial_data.rds"))
  expect_false(
    validate_consolidated_financial(example_data[-1])
  )
})

test_that("validate_consolidated_financial() returns FALSE for data with an unspecified column", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/consolidated_financial_data.rds"))
  example_data$XXX = TRUE
  expect_false(
    validate_consolidated_financial(example_data)
  )
})

test_that("validate_consolidated_financial() returns FALSE for data with a column of a different type", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/consolidated_financial_data.rds"))
  example_data[1] <- TRUE
  expect_false(
    validate_consolidated_financial(example_data)
  )
})
