setwd(here::here())


test_that("validate_sector_bridge() function exists", {
  expect_true(
    class(validate_sector_bridge) == "function"
  )
})

test_that("validate_sector_bridge() returns TRUE for example data", {
  expect_true(
    validate_sector_bridge(readRDS("inst/extdata/sector_bridge.rds"))
  )
})

test_that("validate_sector_bridge() returns FALSE for data with no columns", {
  expect_false(
    validate_sector_bridge(data.frame())
  )
})

test_that("validate_sector_bridge() returns FALSE for data with a specified column missing", {
  expect_false(
    validate_sector_bridge(readRDS("inst/extdata/sector_bridge.rds")[-3])
  )
})

test_that("validate_sector_bridge() returns FALSE for data with an unspecified column", {
  expect_false({
    example_data <- readRDS("inst/extdata/sector_bridge.rds")
    example_data$XXX <- TRUE
    validate_sector_bridge(example_data)
  })
})

test_that("validate_sector_bridge() returns FALSE for data with a column of a different type", {
  expect_false({
    example_data <- readRDS("inst/extdata/sector_bridge.rds")
    example_data$sector <- 1L
    validate_sector_bridge(example_data)
  })
})
