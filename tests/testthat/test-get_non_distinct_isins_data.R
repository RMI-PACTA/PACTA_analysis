test_that("get_non_distinct_isins_data() function exists", {
  expect_true(
    class(get_non_distinct_isins_data) == "function"
  )
})

test_that("get_non_distinct_isins_data() returns a data.frame", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_non_distinct_isins_data()
  expect_true(
    inherits(result, "data.frame")
  )
})

test_that("get_non_distinct_isins_data() returns a data.frame with 1 column", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_non_distinct_isins_data()
  expect_true(
    ncol(result) == 1
  )
})

test_that("get_non_distinct_isins_data() returns a data.frame with the proper column names", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_non_distinct_isins_data()
  expect_equal(
    names(result),
    c("isin")
  )
})

test_that("get_non_distinct_isins_data() returns a data.frame with proper character class columns", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_non_distinct_isins_data()
  char_cols <- c("isin")
  expect_true(
    all(sapply(result[char_cols], class) == "character")
  )
})
