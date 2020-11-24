setwd(here::here())

test_that("get_and_clean_fund_data() function exists", {
  expect_true(
    class(get_and_clean_fund_data) == "function"
  )
})

test_that("get_and_clean_fund_data() returns a data.frame", {
  result <- get_and_clean_fund_data(path = "inst/extdata",
                                    filename = "fund_data_2019Q4.rds")
  expect_true(
    inherits(result, "data.frame")
  )
})
