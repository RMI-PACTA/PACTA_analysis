test_that("with readr < 1.4.0 and named argument `file` throws no error", {
  # Avoid verbose message of `skip_if()`
  if (!utils::packageVersion("readr") >= "1.4.0") {
    x <- tibble::tibble(x = 1)
    file <- tempfile()

    none <- NA
    expect_error(write_csv_file(x = x, file = file), none)
  }

  # Avoid message "Reason: empty test"
  expect_true(TRUE)
})
