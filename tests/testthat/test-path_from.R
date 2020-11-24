test_that("is sensitive to `from`", {
  expect_equal(
    path_from("a", "b", "c"),
    here::here("a", "b", "c")
  )

  expect_equal(
    path_from(),
    here::here()
  )
})
