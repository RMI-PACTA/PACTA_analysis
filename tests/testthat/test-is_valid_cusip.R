test_that("is_valid_cusip() returns `FALSE` for a blank string", {
  expect_false(is_valid_cusip(""))
})

test_that("is_valid_cusip() returns `FALSE` for a single space", {
  expect_false(is_valid_cusip(" "))
})

test_that("is_valid_cusip() returns `FALSE` for a string with not enough characters", {
  expect_false(is_valid_cusip("xxx"))
})

test_that("is_valid_cusip() returns `FALSE` for a string with an invalid character", {
  expect_false(is_valid_cusip("xxxx$xxxx"))
})

test_that("is_valid_cusip() returns `FALSE` for a string with an invalid check digit", {
  expect_false(is_valid_cusip("xxxxxxxxx"))
})

test_that("is_valid_cusip() returns `FALSE` for a string with a valid check digit, but an invalid code", {
  expect_false(is_valid_cusip("xxxxxxxx9"))
})

test_that("is_valid_cusip() returns `TRUE` for a valid CUSIP", {
  expect_true(is_valid_cusip("36202D2T7"))
})

test_that("is_valid_cusip() returns `TRUE` for a valid CUSIP with a weird character", {
  expect_true(is_valid_cusip("37875*AA2"))
})

test_that("is_valid_cusip() returns `TRUE` for a valid CUSIP with a 'S'", {
  expect_true(is_valid_cusip("31403VS59"))
})

test_that("is_valid_cusip() returns `TRUE` for a valid CUSIP with only numbers", {
  expect_true(is_valid_cusip("666762109"))
})

test_that("is_valid_cusip() returns `TRUE` for a valid CUSIP passed as a numeric", {
  expect_true(is_valid_cusip(666762109))
})

test_that("is_valid_cusip() returns the expected value for a character vector", {
  expect_equal(is_valid_cusip(c("36202D2T7", "xxx")), c(TRUE, FALSE))
})

test_that("is_valid_cusip() returns the expected value for a data frame with a single column", {
  expect_equal(is_valid_cusip(data.frame(x = c("36202D2T7", "xxx"))), c(TRUE, FALSE))
})

test_that("is_valid_cusip() returns the expected value for a tibble with a single column", {
  expect_equal(is_valid_cusip(tibble::tibble(x = c("36202D2T7", "xxx"))), c(TRUE, FALSE))
})
