test_that("get_company_emissions() function exists", {
  expect_true(
    class(get_company_emissions) == "function"
  )
})

test_that("get_company_emissions() uses a function name that conforms to the list of data_object_names", {
  expect_true(
    sub("^get_", "", deparse(substitute(get_company_emissions))) %in% data_object_names
  )
})

test_that("get_company_emissions() returns a data.frame", {
  skip_check_but_run_test()
  result <- get_company_emissions(here::here("inst/extdata"))
  expect_true(
    inherits(result, "data.frame")
  )
})

test_that("get_company_emissions() returns a data.frame with 11 columns", {
  skip_check_but_run_test()
  result <- get_company_emissions(here::here("inst/extdata"))
  expect_true(
    ncol(result) == 11
  )
})

test_that("get_company_emissions() returns a data.frame with the proper column names", {
  skip_check_but_run_test()
  result <- get_company_emissions(here::here("inst/extdata"))
  expect_equal(
    names(result),
    c("company_id", "company_name", "ald_sector", "bics_sector",
      "bics_subgroup", "mapped_sector", "unit", "emissions_datastore",
      "emissions_trucost", "emissions", "source")
  )
})

test_that("get_company_emissions() returns a data.frame with proper character class columns", {
  skip_check_but_run_test()
  result <- get_company_emissions(here::here("inst/extdata"))
  char_cols <- c("company_name", "ald_sector", "bics_sector", "bics_subgroup",
                 "mapped_sector", "unit", "source")
  expect_true(
    all(sapply(result[char_cols], class) == "character")
  )
})

test_that("get_company_emissions() returns a data.frame with proper numeric class columns", {
  skip_check_but_run_test()
  result <- get_company_emissions(here::here("inst/extdata"))
  char_cols <- c("company_id", "emissions_datastore", "emissions_trucost",
                 "emissions")
  expect_true(
    all(sapply(result[char_cols], class) == "numeric")
  )
})
