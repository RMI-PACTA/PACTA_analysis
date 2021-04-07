test_that("with readr < 1.4.0 and named argument `file` throws no error", {
  skip_if(utils::packageVersion("readr") >= "1.4.0")

  x <- tibble::tibble(x = 1)
  file <- tempfile()
  expect_error(write_csv_file(x = x, file = file), NA)
})
