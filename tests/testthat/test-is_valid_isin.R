test_that("always outputs a logical vector", {
  # invalid and valid codes
  isins <- c(invalid_isin = "XXX", valid_isin = "US0378331005")
  expect_vector(is_valid_isin(isins[1L]), ptype = logical(), size = 1L)
  expect_vector(is_valid_isin(isins[2L]), ptype = logical(), size = 1L)
  expect_vector(is_valid_isin(isins), ptype = logical(), size = 2L)

  # expected possible uses
  isins_df <- data.frame(isin = isins)

  out <- dplyr::mutate(isins_df, valid_isin = is_valid_isin(isin))$valid_isin
  expect_vector(out, ptype = logical(), size = 2L)

  out <- is_valid_isin(isins_df$isin)
  expect_vector(out, ptype = logical(), size = 2L)

  out <- is_valid_isin(isins_df["isin"])
  expect_vector(out, ptype = logical(), size = 2L)

  out <- is_valid_isin(isins_df[1L])
  expect_vector(out, ptype = logical(), size = 2L)

  out <- is_valid_isin(isins_df[["isin"]])
  expect_vector(out, ptype = logical(), size = 2L)

  out <- is_valid_isin(isins_df[[1L]])
  expect_vector(out, ptype = logical(), size = 2L)

  # unexpected input types
  expect_vector(is_valid_isin(1L), ptype = logical(), size = 1L)
  expect_vector(is_valid_isin(1L:2L), ptype = logical(), size = 2L)
  expect_vector(is_valid_isin(TRUE), ptype = logical(), size = 1L)
  expect_vector(is_valid_isin(FALSE), ptype = logical(), size = 1L)
  expect_vector(is_valid_isin(c(TRUE, FALSE)), ptype = logical(), size = 2L)
  expect_vector(is_valid_isin(NA), ptype = logical(), size = 1L)
  expect_vector(is_valid_isin(c(NA, NA)), ptype = logical(), size = 2L)
})
