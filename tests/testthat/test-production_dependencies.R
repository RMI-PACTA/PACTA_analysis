test_that("production dependencies are up to date", {
  expect_equal(
    production_dependencies(),
    detected_dependencies(exclude = not_for_production())
  )
})
