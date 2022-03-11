test_that("is_valid_cusip", {
  expect_false(is_valid_cusip(""))
  expect_false(is_valid_cusip(" "))
  expect_false(is_valid_cusip("xxx"))  # not enough characters
  expect_false(is_valid_cusip("xxxx$xxxx"))  # invalid character
  expect_false(is_valid_cusip("xxxxxxxxx"))  # invalid check digit
  expect_false(is_valid_cusip("xxxxxxxx9"))  # valid check digit, invalid code
  expect_true(is_valid_cusip("36202D2T7"))
  expect_true(is_valid_cusip("37875*AA2"))  # valid CUSIP with weird character
  expect_true(is_valid_cusip("31403VS59"))  # valid CUSIP with 'S'
  expect_true(is_valid_cusip("666762109"))  # valid CUSIP with only numbers
  expect_true(is_valid_cusip(666762109))  # valid CUSIP passed as numeric
  expect_equal(is_valid_cusip(c("36202D2T7", "xxx")), c(TRUE, FALSE))
  expect_equal(is_valid_cusip(data.frame(x = c("36202D2T7", "xxx"))), c(TRUE, FALSE))
})

