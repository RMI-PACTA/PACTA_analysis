test_that("get_average_sector_intensity() function exists", {
  expect_true(
    class(get_average_sector_intensity) == "function"
  )
})

test_that("get_average_sector_intensity() uses a function name that conforms to the list of data_object_names", {
  expect_true(
    sub("^get_", "", deparse(substitute(get_average_sector_intensity))) %in% data_object_names
  )
})

test_that("get_average_sector_intensity() returns a data.frame", {
  skip_check_but_run_test()
  result <- get_average_sector_intensity(here::here("inst/extdata"))
  expect_true(
    inherits(result, "data.frame")
  )
})

test_that("get_average_sector_intensity() returns a data.frame with 6 columns", {
  skip_check_but_run_test()
  result <- get_average_sector_intensity(here::here("inst/extdata"))
  expect_true(
    ncol(result) == 6
  )
})

test_that("get_average_sector_intensity() returns a data.frame with the proper column names", {
  skip_check_but_run_test()
  result <- get_average_sector_intensity(here::here("inst/extdata"))
  expect_equal(
    names(result),
    c("bics_sector", "mean_intensity", "median_intensity", "sd_intensity", "asset_type", "unit")
  )
})

test_that("get_average_sector_intensity() returns a data.frame with proper character class columns", {
  skip_check_but_run_test()
  result <- get_average_sector_intensity(here::here("inst/extdata"))
  char_cols <- c("bics_sector", "asset_type", "unit")
  expect_true(
    all(sapply(result[char_cols], class) == "character")
  )
})


test_that("get_average_sector_intensity() returns a data.frame with proper numeric class columns", {
  skip_check_but_run_test()
  result <- get_average_sector_intensity(here::here("inst/extdata"))
  char_cols <- c("mean_intensity", "median_intensity", "sd_intensity")
  expect_true(
    all(sapply(result[char_cols], class) == "numeric")
  )
})
