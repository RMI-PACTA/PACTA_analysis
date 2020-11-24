test_that("validate_average_sector_intensity_data() function exists", {
  expect_true(
    class(validate_average_sector_intensity_data) == "function"
  )
})

test_that("validate_average_sector_intensity_data() returns TRUE for example data", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/average_sector_intensity.rds"))
  expect_true(
    validate_average_sector_intensity_data(example_data)
  )
})

test_that("validate_average_sector_intensity_data() returns FALSE for data with no columns", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/average_sector_intensity.rds"))
  expect_false(
    validate_average_sector_intensity_data(data.frame())
  )
})

test_that("validate_average_sector_intensity_data() returns FALSE for data with a specified column missing", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/average_sector_intensity.rds"))
  expect_false(
    validate_average_sector_intensity_data(example_data[-1])
  )
})

test_that("validate_average_sector_intensity_data() returns FALSE for data with an unspecified column", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/average_sector_intensity.rds"))
  example_data$XXX = TRUE
  expect_false(
    validate_average_sector_intensity_data(example_data)
  )
})

test_that("validate_average_sector_intensity_data() returns FALSE for data with a column of a different type", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/average_sector_intensity.rds"))
  example_data[1] <- TRUE
  expect_false(
    validate_average_sector_intensity_data(example_data)
  )
})
