example_data <- readRDS(here::here("inst/extdata/sector_bridge.rds"))

test_that("validate_sector_bridge() function exists", {
  expect_true(
    class(validate_sector_bridge) == "function"
  )
})

test_that("validate_sector_bridge() returns TRUE for example data", {
  expect_true(
    validate_sector_bridge(example_data)
  )
})

test_that("validate_sector_bridge() returns FALSE for data with no columns", {
  expect_false(
    validate_sector_bridge(data.frame())
  )
})

test_that("validate_sector_bridge() returns FALSE for data with a specified column missing", {
  expect_false(
    validate_sector_bridge(example_data[-3])
  )
})

test_that("validate_sector_bridge() returns FALSE for data with an unspecified column", {
  expect_false({
    example_data_local <- example_data
    example_data_local$XXX = TRUE
    validate_sector_bridge(example_data_local)
  })
})

test_that("validate_sector_bridge() returns FALSE for data with a column of a different type", {
  expect_false({
    example_data_local <- example_data
    example_data_local[1] <- TRUE
    validate_sector_bridge(example_data_local)
  })
})
