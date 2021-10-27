test_that("helper skip_check_but_run_test() works as expected", {
  # This test should not be skipped during devtools::test()
  # This test should be skipped during devtools::check()
  # If you remove `skip_check_but_run_test()`, devtools::check() should error
  skip_check_but_run_test()

  # This file should be available only when the source code is accessible, i.e.
  # not during R CMD check, when the code runs from the system installation
  expect_true(fs::file_exists(here::here(".pacta")))
})
