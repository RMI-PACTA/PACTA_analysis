test_that("stop_on_error stops on error", {
  expect_error(stop_on_error(exit_code = 0, script_id = 1), NA)
  expect_error(stop_on_error(exit_code = -1, script_id = 2), NA)
  expect_error(stop_on_error(exit_code = 1, script_id = 1), "1.*error")
  expect_error(stop_on_error(exit_code = 1, script_id = 99), "99.*error")
  expect_error(stop_on_error(exit_code = 3, script_id = 2), "2.*error")
})
