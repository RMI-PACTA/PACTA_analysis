setwd(here::here())
function_name <- "validate_is_dataframe"


test_that(paste0(function_name, "() function exists"), {
  expect_true(
    exists(function_name)
  )
})

test_that(paste0(function_name, "() returns TRUE if the data is a data frame"), {
  expect_true(
    validate_is_dataframe(data.frame())
  )
})

test_that(paste0(function_name, "() returns TRUE if the data is a tibble"), {
  expect_true(
    validate_is_dataframe(tibble())
  )
})

test_that(paste0(function_name, "() returns FALSE if the data is a list"), {
  expect_false(
    validate_is_dataframe(list())
  )
})

test_that(paste0(function_name, "() returns FALSE if the data is a vector"), {
  expect_false(
    validate_is_dataframe(vector())
  )
})

test_that(paste0(function_name, "() returns FALSE if the data is a string"), {
  expect_false(
    validate_is_dataframe("test")
  )
})

test_that(paste0(function_name, "() returns FALSE if the data is a numeric value"), {
  expect_false(
    validate_is_dataframe(1234)
  )
})
