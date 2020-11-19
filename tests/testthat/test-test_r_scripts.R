test_that("stop_on_error stops on error", {
  expect_error(stop_on_error(exit_code = 0), NA)
  expect_error(stop_on_error(exit_code = -1), NA)
  expect_error(stop_on_error(exit_code = 1), "error")
  expect_error(stop_on_error(exit_code = 99), "error")
})
