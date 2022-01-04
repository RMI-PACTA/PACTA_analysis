test_that("outputs logical vector", {
  codes <- c(invalid_code = "XXX", valid_code = "USD")
  expect_vector(is_valid_currency_code(codes[1]), ptype = logical(), size = 1)
  expect_vector(is_valid_currency_code(codes[2]), ptype = logical(), size = 1)
  expect_vector(is_valid_currency_code(codes), ptype = logical(), size = 2)

  codes_df <- data.frame(code = codes)
  expect_vector(dplyr::mutate(codes_df, valid_code = is_valid_currency_code(code))$valid_code, ptype = logical(), size = 2)
  expect_vector(is_valid_currency_code(codes_df$code), ptype = logical(), size = 2)
  expect_vector(is_valid_currency_code(codes_df["code"]), ptype = logical(), size = 2)
  expect_vector(is_valid_currency_code(codes_df[1]), ptype = logical(), size = 2)
  expect_vector(is_valid_currency_code(codes_df[["code"]]), ptype = logical(), size = 2)
  expect_vector(is_valid_currency_code(codes_df[[1]]), ptype = logical(), size = 2)

  expect_vector(is_valid_currency_code(1:2), ptype = logical(), size = 2)
  expect_vector(is_valid_currency_code(c(TRUE, FALSE)), ptype = logical(), size = 2)
  expect_vector(is_valid_currency_code(c(NA, NA)), ptype = logical(), size = 2)
})
