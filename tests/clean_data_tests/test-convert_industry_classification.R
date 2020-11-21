setwd(here::here())
function_name <- "convert_industry_classification"
cat("\ntest\ntest\ntest\ntest\ntest\ntest\ntest\ntest\ntest\ntest\ntest\ntest\n")


test_that(paste0(function_name, "() function exists"), {
  expect_true(
    exists(function_name)
  )
})

test_that(paste0(function_name, "() returns NA for an unknown string"), {
  expect_true(
    is.na(convert_industry_classification("XXX", from = "BICS", to = "sector"))
  )
})

test_that(paste0(function_name, "() returns an appropriately matched string for an exisitng BICS group"), {
  expect_true(
    convert_industry_classification("Advertising Agencies", from = "BICS", to = "sector") == "Other"
  )
})

test_that(paste0(function_name, "() returns an appropriately matched string for an exisitng ICB group"), {
  expect_true(
    convert_industry_classification("Aerospace", from = "ICB", to = "sector") == "Other"
  )
})
