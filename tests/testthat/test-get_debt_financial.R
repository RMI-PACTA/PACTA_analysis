test_that("get_debt_financial() function exists", {
  expect_true(
    class(get_debt_financial) == "function"
  )
})

test_that("get_debt_financial() uses a function name that conforms to the list of data_object_names", {
  expect_true(
    sub("^get_", "", deparse(substitute(get_debt_financial))) %in% data_object_names
  )
})

test_that("get_debt_financial() returns a data.frame", {
  skip_check_but_run_test()
  result <- get_debt_financial(here::here("inst/extdata"))
  expect_true(
    inherits(result, "data.frame")
  )
})

test_that("get_debt_financial() returns a data.frame with 4 columns", {
  skip_check_but_run_test()
  result <- get_debt_financial(here::here("inst/extdata"))
  expect_true(
    ncol(result) == 4
  )
})

test_that("get_debt_financial() returns a data.frame with the proper column names", {
  skip_check_but_run_test()
  result <- get_debt_financial(here::here("inst/extdata"))
  expect_equal(
    names(result),
    c("corporate_bond_ticker", "has_asset_level_data", "sectors_with_assets", "bics_sector")
  )
})

test_that("get_debt_financial() returns a data.frame with proper character class columns", {
  skip_check_but_run_test()
  result <- get_debt_financial(here::here("inst/extdata"))
  char_cols <- c("corporate_bond_ticker", "sectors_with_assets", "bics_sector")
  expect_true(
    all(sapply(result[char_cols], class) == "character")
  )
})

test_that("get_debt_financial() returns a data.frame with proper logical class columns", {
  skip_check_but_run_test()
  result <- get_debt_financial(here::here("inst/extdata"))
  log_cols <- c("has_asset_level_data")
  expect_true(
    all(sapply(result[log_cols], class) == "logical")
  )
})
