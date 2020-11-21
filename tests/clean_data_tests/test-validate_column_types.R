setwd(here::here())
function_name <- "validate_column_types"


test_that(paste0(function_name, "() function exists"), {
  expect_true(
    exists(function_name)
  )
})

test_that(paste0(function_name, "() returns TRUE if column types in the data match the specification"), {
  expect_true(
    validate_column_types(data.frame(a = 1, b = "b"), c(a = "numeric", b = "character"))
  )
})

test_that(paste0(function_name, "() returns TRUE if all column types in the specification match in the data, even when there are additional columns in the data"), {
  expect_true(
    validate_column_types(data.frame(a = 1, b = "b", c = TRUE), c(a = "numeric", b = "character"))
  )
})

test_that(paste0(function_name, "() returns FALSE if a column type in the specification does not match in the data"), {
  expect_false(
    validate_column_types(data.frame(a = 1, b = 1), c(a = "numeric", b = "character"))
  )
})

test_that(paste0(function_name, "() returns FALSE if a column type in the specification is not found in the data"), {
  expect_false(
    validate_column_types(data.frame(a = 1), c(a = "numeric", b = "character"))
  )
})
