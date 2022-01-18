test_that("skip_if_R_CMD_check work", {
  # Test that a skip happens
  withr::local_envvar(R_CMD = "yes")
  expect_condition(skip_if_R_CMD_check(), class = "skip")

  # Test that a skip doesn't happen
  withr::local_envvar(R_CMD = "")
  expect_condition(skip_if_R_CMD_check(), NA, class = "skip")
})
