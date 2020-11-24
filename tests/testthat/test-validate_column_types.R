test_that("validate_column_types() function exists", {
  expect_true(
    class(validate_column_types) == "function"
  )
})

test_that("validate_column_types() returns TRUE if column types in the data match the specification", {
  expect_true(
    validate_column_types(data.frame(a = 1, b = "b"), c(a = "numeric", b = "character"))
  )
})

test_that("validate_column_types() returns TRUE if all column types in the specification match in the data, even when there are additional columns in the data", {
  expect_true(
    validate_column_types(data.frame(a = 1, b = "b", c = TRUE), c(a = "numeric", b = "character"))
  )
})

test_that("validate_column_types() returns FALSE if a column type in the specification does not match in the data", {
  expect_false(
    validate_column_types(data.frame(a = 1, b = 1), c(a = "numeric", b = "character"))
  )
})

test_that("validate_column_types() returns FALSE if a column type in the specification is not found in the data", {
  expect_false(
    validate_column_types(data.frame(a = 1), c(a = "numeric", b = "character"))
  )
})
