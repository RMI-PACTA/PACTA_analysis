test_that("validate_revenue() function exists", {
  expect_true(
    class(validate_revenue) == "function"
  )
})

test_that("validate_revenue() uses a function name that conforms to the list of data_object_names", {
  expect_true(
    sub("^validate_", "", deparse(substitute(validate_revenue))) %in% data_object_names
  )
})

test_that("validate_revenue() returns TRUE for example data", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/revenue.rds"))
  expect_true(
    validate_revenue(example_data)
  )
})

test_that("validate_revenue() returns FALSE for data with no columns", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/revenue.rds"))
  expect_false(
    validate_revenue(data.frame())
  )
})

test_that("validate_revenue() returns FALSE for data with a specified column missing", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/revenue.rds"))
  expect_false(
    validate_revenue(example_data[-1])
  )
})

test_that("validate_revenue() returns FALSE for data with an unspecified column", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/revenue.rds"))
  example_data$XXX <- TRUE
  expect_false(
    validate_revenue(example_data)
  )
})

test_that("validate_revenue() returns FALSE for data with a column of a different type", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/revenue.rds"))
  example_data[1] <- TRUE
  expect_false(
    validate_revenue(example_data)
  )
})
