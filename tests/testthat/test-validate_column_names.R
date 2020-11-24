test_that("validate_only_column_names() function exists", {
  expect_true(
    class(validate_only_column_names) == "function"
  )
})

test_that("validate_only_column_names() returns TRUE if all columns in the data match the specification", {
  expect_true(
    validate_only_column_names(data.frame(a = 1, b = "b"), c(a = "numeric", b = "character"))
  )
})

test_that("validate_only_column_names() returns TRUE if all columns in the data match the specification, but not all of the columns in the specification are found in the data", {
  expect_true(
    validate_only_column_names(data.frame(a = 1), c(a = "numeric", b = "character"))
  )
})

test_that("validate_only_column_names() returns FALSE if the data has columns that are not in the specification", {
  expect_false(
    validate_only_column_names(data.frame(a = 1, b = "b", c = 1), c(a = "numeric", b = "character"))
  )
})

test_that("validate_only_column_names() returns FALSE if the data does not have any of the columns that are in the specification", {
  expect_false(
    validate_only_column_names(data.frame(c = 1, d = "b"), c(a = "numeric", b = "character"))
  )
})
