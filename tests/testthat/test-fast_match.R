test_that("fast_match() function exists", {
  expect_true(
    class(fast_match) == "function"
  )
})

test_that("fast_match() function properly matches a minimal example", {
  x <- c("A", "B")
  dict <- data.frame(from = c("A", "B"), to = c("a", "b"))
  expect_true(
    identical(fast_match(x = x, dict = dict), c("a", "b"))
  )
})
