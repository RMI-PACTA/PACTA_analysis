test_that("stop_on_error stops on error", {
  expect_error(stop_on_error(exit_code = 0), NA)
  expect_error(stop_on_error(exit_code = -1), NA)
  expect_error(stop_on_error(exit_code = 1), "error")
  expect_error(stop_on_error(exit_code = 99), "error")
})

test_that("example data is available under sample_data/", {
  # TODO: Make this available in inst/extdate with an example helper?
  # -- https://git.io/JkX5O
  example_data <- here::here(
    "sample_files", "20_input_files", "TestPortfolio_Input.csv"
  )
  expect_true(fs::file_exists(example_data))
})

test_that("example data is available under working_dir/", {
  # FIXME: Replace this with a a function that gets the path from
  # working_dir/10_Parameter_File/TestPortfolio_Input_PortfolioParameters.yml
  # portfolio_name_in: TestPortfolio_Input
  # -- https://git.io/JkX5O
  example_data <- here::here(
    "working_dir", "20_Raw_Inputs", "TestPortfolio_Input.csv"
  )
  expect_true(fs::file_exists(example_data))
})

