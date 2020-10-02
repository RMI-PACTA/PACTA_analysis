test_that("outputs the input, invisibly, as it's called for a side effect", {
  rmd <- tempfile("input", fileext = ".Rmd")
  writeLines("A file with some text", rmd)

  expect_invisible(test_integration(rmd))
  expect_equal(test_integration(rmd), rmd)
})
