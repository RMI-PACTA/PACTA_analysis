result <- get_average_sector_intensity_data(here::here("inst/extdata"))

test_that("get_average_sector_intensity_data() function exists", {
  expect_true(
    class(get_average_sector_intensity_data) == "function"
  )
})

test_that("get_average_sector_intensity_data() returns a data.frame", {
  expect_true(
    inherits(result, "data.frame")
  )
})

test_that("get_average_sector_intensity_data() returns a data.frame with 6 columns", {
  expect_true(
    ncol(result) == 6
  )
})

test_that("get_average_sector_intensity_data() returns a data.frame with the proper column names", {
  expect_equal(
    names(result),
    c("bics_sector", "mean_intensity", "median_intensity", "sd_intensity", "asset_type", "unit")
  )
})

test_that("get_average_sector_intensity_data() returns a data.frame with proper character class columns", {
  char_cols <- c("bics_sector", "asset_type", "unit")
  expect_true(
    all(sapply(result[char_cols], class) == "character")
  )
})


test_that("get_average_sector_intensity_data() returns a data.frame with proper numeric class columns", {
  char_cols <- c("mean_intensity", "median_intensity", "sd_intensity")
  expect_true(
    all(sapply(result[char_cols], class) == "numeric")
  )
})
