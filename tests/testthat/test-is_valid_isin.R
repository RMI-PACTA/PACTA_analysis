test_that("outputs logical vector", {
  isins <- c(invalid_isin = "XXX", valid_isin = "US0378331005")
  expect_vector(is_valid_isin(isins[1L]), ptype = logical(), size = 1L)
  expect_vector(is_valid_isin(isins[2L]), ptype = logical(), size = 1L)
  expect_vector(is_valid_isin(isins), ptype = logical(), size = 2L)

  isins_df <- data.frame(isin = isins)
  expect_vector(dplyr::mutate(isins_df, valid_isin = is_valid_isin(isin))$valid_isin, ptype = logical(), size = 2L)
  expect_vector(is_valid_isin(isins_df$isin), ptype = logical(), size = 2L)
  expect_vector(is_valid_isin(isins_df["isin"]), ptype = logical(), size = 2L)
  expect_vector(is_valid_isin(isins_df[1L]), ptype = logical(), size = 2L)
  expect_vector(is_valid_isin(isins_df[["isin"]]), ptype = logical(), size = 2L)
  expect_vector(is_valid_isin(isins_df[[1L]]), ptype = logical(), size = 2L)

  expect_vector(is_valid_isin(1L:2L), ptype = logical(), size = 2L)
  expect_vector(is_valid_isin(c(TRUE, FALSE)), ptype = logical(), size = 2L)
  expect_vector(is_valid_isin(c(NA, NA)), ptype = logical(), size = 2L)
})
