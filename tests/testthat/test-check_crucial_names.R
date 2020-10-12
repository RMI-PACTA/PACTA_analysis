test_that("check_crucial_names with expected names returns `x` invisibly", {
  x <- c(a = 1)
  expect_silent(out <- check_crucial_names(x, "a"))
  expect_identical(x, out)

  x <- data.frame(a = 1)
  expect_silent(out <- check_crucial_names(x, "a"))
  expect_identical(x, out)
})

test_that("check_crucial_names with expected names returns `x` invisibly", {
  x <- c(a = 1)
  expect_error(check_crucial_names(x, "b"), class = "missing_names")

  x <- data.frame(a = 1)
  expect_error(check_crucial_names(x, "b"), class = "missing_names")
})

test_that("check_crucial_names w/ duplicated expected informs unique", {
  expect_error(
    check_crucial_names(c(x = 1), expected_names = c("x", "z", "a")),
    class = "missing_names"
  )
})
