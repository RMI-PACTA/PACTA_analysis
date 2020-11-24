test_that("validate_has_column_that_matches() function exists", {
  expect_true(
    class(validate_has_column_that_matches) == "function"
  )
})

test_that("validate_has_column_that_matches() returns TRUE if the data contains a column name that matches the regex", {
  expect_true(
    validate_has_column_that_matches(data.frame(a2 = 1), "[a-z][0-9]")
  )
})

test_that("validate_has_column_that_matches() returns TRUE if the data contains more than one column name that matches the regex", {
  expect_true(
    validate_has_column_that_matches(data.frame(a2 = 1, b3 = 1), "[a-z][0-9]")
  )
})

test_that("validate_has_column_that_matches() returns FALSE if the data does not contain a column name that matches the regex", {
  expect_false(
    validate_has_column_that_matches(data.frame(aa = 1), "[a-z][0-9]")
  )
})
