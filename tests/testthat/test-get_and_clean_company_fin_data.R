test_that("get_and_clean_company_fin_data() function exists", {
  expect_true(
    class(get_and_clean_company_fin_data) == "function"
  )
})

test_that("get_and_clean_company_fin_data() returns a data.frame", {
  skip_check_but_run_test()
  setwd(here::here())
  expect_true(
    inherits(get_and_clean_company_fin_data("inst/extdata"), "data.frame")
  )
})
