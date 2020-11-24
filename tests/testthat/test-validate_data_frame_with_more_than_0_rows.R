test_that("validate_data_frame_with_more_than_0_rows() function exists", {
  expect_true(
    class(validate_data_frame_with_more_than_0_rows) == "function"
  )
})

test_that("validate_data_frame_with_more_than_0_rows() returns TRUE if the object is a data frame with more than 0 rows", {
  expect_true(
    validate_data_frame_with_more_than_0_rows(data.frame(a = T))
  )
})

test_that("validate_data_frame_with_more_than_0_rows() returns FALSE if the object is not a data frame", {
  expect_false(
    validate_data_frame_with_more_than_0_rows("a")
  )
})

test_that("validate_data_frame_with_more_than_0_rows() returns FALSE if the object is a data frame with 0 rows", {
  expect_false(
    validate_data_frame_with_more_than_0_rows(data.frame())
  )
})
