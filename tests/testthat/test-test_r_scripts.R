test_that("stop_on_error stops on error", {
  expect_error(stop_on_error(exit_code = 0), NA)
  expect_error(stop_on_error(exit_code = -1), NA)
  expect_error(stop_on_error(exit_code = 1), "error")
  expect_error(stop_on_error(exit_code = 99), "error")
})

test_that("example data is available under working_dir/", {
  # https://git.io/JkX5O
  config <-
    path_from(parameter_file(), "TestPortfolio_Input_PortfolioParameters.yml")
  portfolio_name <- get_param("parameters", "portfolio_name_in")(config)

  example_data <- path_from(raw_inputs(), glue::glue("{portfolio_name}.csv"))
  expect_true(fs::file_exists(example_data))
})

