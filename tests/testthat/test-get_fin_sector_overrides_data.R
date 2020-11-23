setwd(here::here())
function_name <- "get_fin_sector_overrides_data"
result <- get_fin_sector_overrides_data()

test_that(paste0(function_name, "() function exists"), {
  expect_true(
    exists(function_name)
  )
})

test_that(paste0(function_name, "() returns a data.frame"), {
  expect_true(
    inherits(result, "data.frame")
  )
})

test_that(paste0(function_name, "() returns a data.frame with 4 columns"), {
  expect_true(
    ncol(result) == 4
  )
})

test_that(paste0(function_name, "() returns a data.frame with the proper column names"), {
  expect_equal(
    names(result),
    c("company_name", "corporate_bond_ticker", "bloomberg_id",
      "fin_sector_override")
  )
})

test_that(paste0(function_name, "() returns a data.frame with proper character class columns"), {
  char_cols <- c("company_name", "corporate_bond_ticker", "fin_sector_override")
  expect_true(
    all(sapply(result[char_cols], class) == "character")
  )
})

test_that(paste0(function_name, "() returns a data.frame with proper numeric class columns"), {
  char_cols <- c("bloomberg_id")
  expect_true(
    all(sapply(result[char_cols], class) == "numeric")
  )
})
