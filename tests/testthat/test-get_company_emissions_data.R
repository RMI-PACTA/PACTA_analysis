result <- get_company_emissions_data(here::here("inst/extdata"))

test_that("get_company_emissions_data() function exists", {
  expect_true(
    class(get_company_emissions_data) == "function"
  )
})

test_that("get_company_emissions_data() returns a data.frame", {
  expect_true(
    inherits(result, "data.frame")
  )
})

test_that("get_company_emissions_data() returns a data.frame with 11 columns", {
  expect_true(
    ncol(result) == 11
  )
})

test_that("get_company_emissions_data() returns a data.frame with the proper column names", {
  expect_equal(
    names(result),
    c("company_id", "company_name", "ald_sector", "bics_sector",
      "bics_subgroup", "mapped_sector", "unit", "emissions_datastore",
      "emissions_trucost", "emissions", "source")
  )
})

test_that("get_company_emissions_data() returns a data.frame with proper character class columns", {
  char_cols <- c("company_name", "ald_sector", "bics_sector", "bics_subgroup",
                 "mapped_sector", "unit", "source")
  expect_true(
    all(sapply(result[char_cols], class) == "character")
  )
})

test_that("get_company_emissions_data() returns a data.frame with proper numeric class columns", {
  char_cols <- c("company_id", "emissions_datastore", "emissions_trucost",
                 "emissions")
  expect_true(
    all(sapply(result[char_cols], class) == "numeric")
  )
})
