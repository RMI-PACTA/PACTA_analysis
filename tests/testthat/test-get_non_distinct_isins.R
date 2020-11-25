test_that("get_non_distinct_isins() function exists", {
  expect_true(
    class(get_non_distinct_isins) == "function"
  )
})

test_that("get_non_distinct_isins() uses a function name that conforms to the list of data_object_names", {
  expect_true(
    sub("^get_", "", deparse(substitute(get_non_distinct_isins))) %in% data_object_names
  )
})

test_that("get_non_distinct_isins() returns a data.frame", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_non_distinct_isins()
  expect_true(
    inherits(result, "data.frame")
  )
})

test_that("get_non_distinct_isins() returns a data.frame with 1 column", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_non_distinct_isins()
  expect_true(
    ncol(result) == 1
  )
})

test_that("get_non_distinct_isins() returns a data.frame with the proper column names", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_non_distinct_isins()
  expect_equal(
    names(result),
    c("isin")
  )
})

test_that("get_non_distinct_isins() returns a data.frame with proper character class columns", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_non_distinct_isins()
  char_cols <- c("isin")
  expect_true(
    all(sapply(result[char_cols], class) == "character")
  )
})
