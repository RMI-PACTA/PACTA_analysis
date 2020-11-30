test_that("validate_ungrouped() function exists", {
  expect_true(
    class(validate_ungrouped) == "function"
  )
})

test_that("validate_ungrouped() returns TRUE if the data is an ungrouped data frame", {
  expect_true(
    validate_ungrouped(data.frame(a = 1))
  )
})

test_that("validate_ungrouped() returns TRUE if the data is an ungrouped tibble", {
  expect_true(
    validate_ungrouped(tibble(a = 1))
  )
})

test_that("validate_ungrouped() returns FALSE if the data is a grouped tibble", {
  expect_false(
    validate_ungrouped(group_by(tibble(a = 1), a))
  )
})

test_that("validate_ungrouped() can succesffully use an error collector", {
  error_collector <- new_error_collector()
  validate_ungrouped(group_by(tibble(a = 1), a), error_collector = error_collector)
  expect_false(
    error_collector$isEmpty()
  )
})
