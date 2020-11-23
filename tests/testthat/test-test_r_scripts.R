test_that("stop_on_error stops on error", {
  expect_error(stop_on_error(exit_code = 0), NA)
  expect_error(stop_on_error(exit_code = -1), NA)
  expect_error(stop_on_error(exit_code = 1), "error")
  expect_error(stop_on_error(exit_code = 99), "error")
})

test_that("example data is available under sample_data/", {
  # FIXME: @jacobvjk, can you explain the role of this file?
  # At https://bit.ly/35Xypsw I read:
  # > copy TestPortfolio_Input.csv from sample_files/20_input_files/ to
  # > working_dir/20_Raw_Inputs/
  # Do we copy that file manually or programatically? If so, where is the code?
  # My experiments suggest web_tool_script_1.R runs to the end without it, and
  # I need to know it that's okay of if I should throw an error ASAP.
  example_data <- here::here(
    "sample_files", "20_input_files", "TestPortfolio_Input.csv"
  )
  expect_true(fs::file_exists(example_data))
})

test_that("example data is available under sample_data/", {
  # FIXME: When this file if unavailable the error message is unhelpful
  # Error in get_input_files(portfolio_name_ref_all) :
  #   Difference in input files and input argument portfolio names.
  # Execution halted
  # Error: This script threw an error.
  # TODO: Fail fast with a more informative error message.
  example_data <- here::here(
    "working_dir", "20_Raw_Inputs", "TestPortfolio_Input.csv"
  )
  expect_true(fs::file_exists(example_data))
})

