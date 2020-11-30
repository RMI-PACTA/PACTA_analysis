test_that("validate_is_named_character() function exists", {
  expect_true(
    class(validate_is_named_character) == "function"
  )
})

test_that("validate_is_named_character() returns TRUE if the object is a named character vector", {
  expect_true(
    validate_is_named_character(c(a = "a"))
  )
})

test_that("validate_is_named_character() returns FALSE if the object is an unnamed character vector", {
  expect_false(
    validate_is_named_character(c("a"))
  )
})

test_that("validate_is_named_character() returns FALSE if the object is a named non-character vector", {
  expect_false(
    validate_is_named_character(c(a = 1L))
  )
})

test_that("validate_is_named_character() returns FALSE if the object is a data frame", {
  expect_false(
    validate_is_named_character(data.frame(a = "a"))
  )
})

test_that("validate_is_named_character() can succesffully use an error collector", {
  error_collector <- new_error_collector()
  validate_is_named_character(c("a"), error_collector = error_collector)
  expect_false(
    error_collector$isEmpty()
  )
})
