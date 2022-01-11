test_that("always outputs a logical vector", {
  # invalid and valid codes
  codes <- c(invalid_code = "XXX", valid_code = "USD")
  expect_vector(is_valid_currency_code(codes[1L]), ptype = logical(), size = 1L)
  expect_vector(is_valid_currency_code(codes[2L]), ptype = logical(), size = 1L)
  expect_vector(is_valid_currency_code(codes), ptype = logical(), size = 2L)

  # expected possible uses
  codes_df <- data.frame(code = codes)

  out <- dplyr::mutate(codes_df, valid_code = is_valid_currency_code(code))$valid_code
  expect_vector(out, ptype = logical(), size = 2L)

  out <- is_valid_currency_code(codes_df$code)
  expect_vector(out, ptype = logical(), size = 2L)

  out <- is_valid_currency_code(codes_df["code"])
  expect_vector(out, ptype = logical(), size = 2L)

  out <- is_valid_currency_code(codes_df[1L])
  expect_vector(out, ptype = logical(), size = 2L)

  out <- is_valid_currency_code(codes_df[["code"]])
  expect_vector(out, ptype = logical(), size = 2L)

  out <- is_valid_currency_code(codes_df[[1L]])
  expect_vector(out, ptype = logical(), size = 2L)

  # unexpected input types
  expect_vector(is_valid_currency_code(1L), ptype = logical(), size = 1L)
  expect_vector(is_valid_currency_code(1L:2L), ptype = logical(), size = 2L)
  expect_vector(is_valid_currency_code(TRUE), ptype = logical(), size = 1L)
  expect_vector(is_valid_currency_code(FALSE), ptype = logical(), size = 1L)
  expect_vector(is_valid_currency_code(c(TRUE, FALSE)), ptype = logical(), size = 2L)
  expect_vector(is_valid_currency_code(NA), ptype = logical(), size = 1L)
  expect_vector(is_valid_currency_code(c(NA, NA)), ptype = logical(), size = 2L)
})

test_that("returns expected values", {
  # typical usage
  codes <- c(invalid_code = "XXX", valid_code = "USD", na = NA_character_)
  expect_identical(is_valid_currency_code(codes), c(FALSE, TRUE, FALSE))

  # all valid codes
  codes <- na.omit(countrycode::codelist$iso4217c)
  expect_identical(all(is_valid_currency_code(codes)), TRUE)

  # unexpected input types
  expect_identical(is_valid_currency_code(NA), FALSE)
  expect_identical(is_valid_currency_code(NA_character_), FALSE)
  expect_identical(is_valid_currency_code(c(TRUE, FALSE)), c(FALSE, FALSE))
  expect_identical(is_valid_currency_code(1L:2L), c(FALSE, FALSE))
})
