setwd(here::here())
function_name <- "validate_has_column_that_matches"


test_that(paste0(function_name, "() function exists"), {
  expect_true(
    exists(function_name)
  )
})

test_that(paste0(function_name, "() returns TRUE if the data contains a column name that matches the regex"), {
  expect_true(
    validate_has_column_that_matches(data.frame(a2 = 1), "[a-z][0-9]")
  )
})

test_that(paste0(function_name, "() returns TRUE if the data contains more than one column name that matches the regex"), {
  expect_true(
    validate_has_column_that_matches(data.frame(a2 = 1, b3 = 1), "[a-z][0-9]")
  )
})

test_that(paste0(function_name, "() returns FALSE if the data does not contain a column name that matches the regex"), {
  expect_false(
    validate_has_column_that_matches(data.frame(aa = 1), "[a-z][0-9]")
  )
})
