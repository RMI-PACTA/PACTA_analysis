test_that("production_dependencies.R matche those found by renv", {
  skip("FIXME: Figure out why dependencies missmatch")
  expect_equal(
    production_dependencies(),
    detected_dependencies(exclude = not_for_production())
  )
})
