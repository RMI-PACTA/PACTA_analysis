test_that("returns an alpha-sorted character-vector", {
  expect_type(packages(), "character")
  expect_equal(packages(), sort(packages()))
})
