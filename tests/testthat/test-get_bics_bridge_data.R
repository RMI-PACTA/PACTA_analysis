setwd(here::here())
result <- get_bics_bridge_data()

test_that("get_bics_bridge_data() function exists", {
  expect_true(
    class(get_bics_bridge_data) == "function"
  )
})

test_that("get_bics_bridge_data() returns a data.frame", {
  expect_true(
    inherits(result, "data.frame")
  )
})

test_that("get_bics_bridge_data() returns a data.frame with 2 columns", {
  expect_true(
    ncol(result) == 2
  )
})

test_that("get_bics_bridge_data() returns a data.frame with the proper column names", {
  expect_equal(
    names(result),
    c("bics_sector", "bics_subgroup")
  )
})

test_that("get_bics_bridge_data() returns a data.frame with proper character class columns", {
  char_cols <- c("bics_sector", "bics_subgroup")
  expect_true(
    all(sapply(result[char_cols], class) == "character")
  )
})
