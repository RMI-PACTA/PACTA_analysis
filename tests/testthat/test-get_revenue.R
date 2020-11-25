test_that("get_revenue() function exists", {
  expect_true(
    class(get_revenue) == "function"
  )
})

test_that("get_revenue() uses a function name that conforms to the list of data_object_names", {
  expect_true(
    sub("^get_", "", deparse(substitute(get_revenue))) %in% data_object_names
  )
})

test_that("get_revenue() returns a data.frame", {
  skip_check_but_run_test()
  result <- get_revenue(here::here("inst/extdata"))
  expect_true(
    inherits(result, "data.frame")
  )
})

test_that("get_revenue() returns a data.frame with 8 columns", {
  skip_check_but_run_test()
  result <- get_revenue(here::here("inst/extdata"))
  expect_true(
    ncol(result) == 8
  )
})

test_that("get_revenue() returns a data.frame with the proper column names", {
  skip_check_but_run_test()
  result <- get_revenue(here::here("inst/extdata"))
  expect_equal(
    names(result),
    c("company_id", "company_name", "bloomberg_id", "equity_ticker",
      "corporate_bond_ticker", "has_revenue_data", "sector", "tot_rev")
  )
})

test_that("get_revenue() returns a data.frame with proper character class columns", {
  skip_check_but_run_test()
  result <- get_revenue(here::here("inst/extdata"))
  char_cols <- c("company_name", "equity_ticker", "corporate_bond_ticker", "sector")
  expect_true(
    all(sapply(result[char_cols], class) == "character")
  )
})

test_that("get_revenue() returns a data.frame with proper logical class columns", {
  skip_check_but_run_test()
  result <- get_revenue(here::here("inst/extdata"))
  log_cols <- c("has_revenue_data")
  expect_true(
    all(sapply(result[log_cols], class) == "logical")
  )
})

test_that("get_revenue() returns a data.frame with proper numeric class columns", {
  skip_check_but_run_test()
  result <- get_revenue(here::here("inst/extdata"))
  num_cols <- c("company_id", "bloomberg_id", "tot_rev")
  expect_true(
    all(sapply(result[num_cols], class) == "numeric")
  )
})
