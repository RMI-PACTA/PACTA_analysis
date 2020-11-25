test_that("get_funds() function exists", {
  expect_true(
    class(get_funds) == "function"
  )
})

test_that("get_funds() uses a function name that conforms to the list of data_object_names", {
  expect_true(
    sub("^get_", "", deparse(substitute(get_funds))) %in% data_object_names
  )
})

test_that("get_funds() returns a valid funds data.frame", {
  skip_check_but_run_test()
  result <- get_funds(here::here("inst/extdata"), filename = "fund_data_2019Q4.rds")
  expect_true(
    validate_funds(result)
  )
})
