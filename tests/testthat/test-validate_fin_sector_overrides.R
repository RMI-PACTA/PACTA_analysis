test_that("validate_fin_sector_overrides() function exists", {
  expect_true(
    class(validate_fin_sector_overrides) == "function"
  )
})

test_that("validate_fin_sector_overrides() uses a function name that conforms to the list of data_object_names", {
  expect_true(
    sub("^validate_", "", deparse(substitute(validate_fin_sector_overrides))) %in% data_object_names
  )
})

test_that("validate_fin_sector_overrides() returns TRUE for example data", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/fin_sector_overrides.rds"))
  expect_true(
    validate_fin_sector_overrides(example_data)
  )
})

test_that("validate_fin_sector_overrides() returns FALSE for data with no columns", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/fin_sector_overrides.rds"))
  expect_false(
    validate_fin_sector_overrides(data.frame())
  )
})

test_that("validate_fin_sector_overrides() returns FALSE for data with a specified column missing", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/fin_sector_overrides.rds"))
  expect_false(
    validate_fin_sector_overrides(example_data[-1])
  )
})

test_that("validate_fin_sector_overrides() returns FALSE for data with an unspecified column", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/fin_sector_overrides.rds"))
  example_data$XXX <- TRUE
  expect_false(
    validate_fin_sector_overrides(example_data)
  )
})

test_that("validate_fin_sector_overrides() returns FALSE for data with a column of a different type", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/fin_sector_overrides.rds"))
  example_data[1] <- TRUE
  expect_false(
    validate_fin_sector_overrides(example_data)
  )
})
