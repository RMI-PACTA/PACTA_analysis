test_that("validate_sector_bridge() function exists", {
  expect_true(
    class(validate_sector_bridge) == "function"
  )
})

test_that("validate_sector_bridge() returns TRUE for example data", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/sector_bridge.rds"))
  expect_true(
    validate_sector_bridge(example_data)
  )
})

test_that("validate_sector_bridge() returns FALSE for data with no columns", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/sector_bridge.rds"))
  expect_false(
    validate_sector_bridge(data.frame())
  )
})

test_that("validate_sector_bridge() returns FALSE for data with a specified column missing", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/sector_bridge.rds"))
  expect_false(
    validate_sector_bridge(example_data[-3])
  )
})

test_that("validate_sector_bridge() returns FALSE for data with an unspecified column", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/sector_bridge.rds"))
  example_data$XXX = TRUE
  expect_false(
    validate_sector_bridge(example_data)
  )
})

test_that("validate_sector_bridge() returns FALSE for data with a column of a different type", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/sector_bridge.rds"))
  example_data[1] <- TRUE
  expect_false(
    validate_sector_bridge(example_data)
  )
})
