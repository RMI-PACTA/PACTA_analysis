setwd(here::here())
function_name <- "get_and_clean_fund_data"
result <- get_and_clean_fund_data(path = "inst/extdata",
                                  filename = "fund_data_2019Q4.rds")

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
