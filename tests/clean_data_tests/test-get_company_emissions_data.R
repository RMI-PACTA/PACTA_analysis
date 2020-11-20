setwd(here::here())

function_name <- "get_company_emissions_data"

test_that(paste0(function_name, "() function exists"), {
  expect_false(
    is.null(get_company_emissions_data)
  )
})

result <- get_company_emissions_data("tests/clean_data_tests/example_data")

test_that(paste0(function_name, "() returns a data.frame"), {
  expect_true(
    inherits(result, "data.frame")
  )
})

test_that(paste0(function_name, "() returns a data.frame with 11 columns"), {
  expect_true(
    ncol(result) == 11
  )
})

test_that(paste0(function_name, "() returns a data.frame with the proper column names"), {
  expect_equal(
    names(result),
    c("company_id", "company_name", "ald_sector", "bics_sector",
      "bics_subgroup", "mapped_sector", "unit", "emissions_datastore",
      "emissions_trucost", "emissions", "source")
  )
})

test_that(paste0(function_name, "() returns a data.frame with proper character class columns"), {
  char_cols <- c("company_name", "ald_sector", "bics_sector", "bics_subgroup",
                 "mapped_sector", "unit", "source")
  expect_true(
    all(sapply(result[char_cols], class) == "character")
  )
})

test_that(paste0(function_name, "() returns a data.frame with proper numeric class columns"), {
  char_cols <- c("company_id", "emissions_datastore", "emissions_trucost",
                 "emissions")
  expect_true(
    all(sapply(result[char_cols], class) == "numeric")
  )
})
