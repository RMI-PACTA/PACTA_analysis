test_that("returns an alpha-sorted character-vector", {
  expect_is(packages(), "character")
  expect_equal(packages(), sort(packages()))
})
