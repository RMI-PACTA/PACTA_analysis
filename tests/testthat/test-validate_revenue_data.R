example_data <- readRDS(here::here("inst/extdata/revenue_data.rds"))

test_that("validate_revenue_data() function exists", {
  expect_true(
    class(validate_revenue_data) == "function"
  )
})

test_that("validate_revenue_data() returns TRUE for example data", {
  expect_true(
    validate_revenue_data(example_data)
  )
})

test_that("validate_revenue_data() returns FALSE for data with no columns", {
  expect_false(
    validate_revenue_data(data.frame())
  )
})

test_that("validate_revenue_data() returns FALSE for data with a specified column missing", {
  expect_false(
    validate_revenue_data(example_data[-1])
  )
})

test_that("validate_revenue_data() returns FALSE for data with an unspecified column", {
  example_data_local <- example_data
  example_data_local$XXX = TRUE
  expect_false(
    validate_revenue_data(example_data_local)
  )
})

test_that("validate_revenue_data() returns FALSE for data with a column of a different type", {
  example_data_local <- example_data
  example_data_local[1] <- TRUE
  expect_false(
    validate_revenue_data(example_data_local)
  )
})
