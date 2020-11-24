test_that("get_fin_sector_overrides_data() function exists", {
  expect_true(
    class(get_fin_sector_overrides_data) == "function"
  )
})

test_that("get_fin_sector_overrides_data() returns a data.frame", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_fin_sector_overrides_data()
  expect_true(
    inherits(result, "data.frame")
  )
})

test_that("get_fin_sector_overrides_data() returns a data.frame with 4 columns", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_fin_sector_overrides_data()
  expect_true(
    ncol(result) == 4
  )
})

test_that("get_fin_sector_overrides_data() returns a data.frame with the proper column names", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_fin_sector_overrides_data()
  expect_equal(
    names(result),
    c("company_name", "corporate_bond_ticker", "bloomberg_id",
      "fin_sector_override")
  )
})

test_that("get_fin_sector_overrides_data() returns a data.frame with proper character class columns", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_fin_sector_overrides_data()
  char_cols <- c("company_name", "corporate_bond_ticker", "fin_sector_override")
  expect_true(
    all(sapply(result[char_cols], class) == "character")
  )
})

test_that("get_fin_sector_overrides_data() returns a data.frame with proper numeric class columns", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_fin_sector_overrides_data()
  char_cols <- c("bloomberg_id")
  expect_true(
    all(sapply(result[char_cols], class) == "numeric")
  )
})
