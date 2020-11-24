skip_check_but_run_test <- function() {
  testthat::skip_if_not(
    fs::file_exists(here::here(".pacta")),
    "Source code is unavailable"
  )
}
