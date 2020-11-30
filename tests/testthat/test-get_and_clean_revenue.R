test_that("get_and_clean_revenue() function exists", {
  expect_true(
    class(get_and_clean_revenue) == "function"
  )
})

test_that("get_and_clean_revenue() uses a function name that conforms to the list of data_object_names", {
  expect_true(
    sub("^get_and_clean_", "", deparse(substitute(get_and_clean_revenue))) %in% data_object_names
  )
})

test_that("get_and_clean_revenue() returns a data.frame", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_and_clean_revenue(path = "inst/extdata")
  expect_true(
    inherits(result, "data.frame")
  )
})

test_that("get_and_clean_revenue() accepts a custom filename", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_and_clean_revenue(path = "inst/extdata", filename = "revenue.rds")
  expect_true(
    inherits(result, "data.frame")
  )
})
