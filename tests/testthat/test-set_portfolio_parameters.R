test_that("`set_portfolio_parameters()` works as expected", {
  skip_if_R_CMD_check()

  test_set_portfolio_parameters_in_callr <- function(.param) {
    callr::r(function(.params) {
      devtools::load_all(quiet = TRUE)
      filepath <- tempfile(fileext = ".yml")
      yaml::write_yaml(list(default = list(parameters = .params)), filepath)
      set_portfolio_parameters(filepath)

      as.list.environment(.GlobalEnv)
    },
    args = list(parameters)
    )
  }

  # test that standard parameter names are read in to the proper object names in
  # the global environment
  parameters <-
    list(
      portfolio_name = "TestPortfolio",
      investor_name = "TestInvestor",
      peer_group = "test_group",
      language = "DE",
      user_id = "2345",
      project_code = "GENERAL",
      holdings_date = "2020Q4"
    )

  out <- test_set_portfolio_parameters_in_callr(parameters)
  expect_equal(out$portfolio_name, "TestPortfolio")
  expect_equal(out$investor_name, "TestInvestor")
  expect_equal(out$peer_group, "test_group")
  expect_equal(out$language_select, "DE")
  expect_equal(out$user_id, "2345")
  expect_equal(out$project_code, "GENERAL")
  expect_equal(out$port_holdings_date, "2020Q4")

  # test that non-standard parameter names that have been used in the past are
  # still read in and set to the proper object names in the global environment
  parameters <- list(
    portfolio_name_in = "TestPortfolio",
    investor_name_in = "TestInvestor"
  )

  out <- test_set_portfolio_parameters_in_callr(parameters)
  expect_equal(out$portfolio_name, "TestPortfolio")
  expect_equal(out$investor_name, "TestInvestor")

  # test that if necessary parameters are unset in the config file the proper
  # object names will be set to NULL in the global environment
  parameters <- list()

  out <- test_set_portfolio_parameters_in_callr(parameters)
  expect_null(out$portfolio_name)
  expect_null(out$investor_name)
  expect_null(out$peer_group)
  expect_null(out$language_select)
  expect_null(out$user_id)
  expect_null(out$project_code)
  expect_null(out$port_holdings_date)

  # test that when holdings_date is set to a list of values that are the same
  # value, the port_holdings_date in the global environment will be set to a
  # vector of those same multiple values
  parameters <- list(holdings_date = c("2020Q4", "2020Q4"))

  out <- test_set_portfolio_parameters_in_callr(parameters)
  expect_equal(out$port_holdings_date, c("2020Q4", "2020Q4"))

  # test that when holdings_date is set to a list of values that are different,
  # the port_holdings_date in the global environment will be set to a vector of
  # those same different values
  parameters <- list(holdings_date = c("2019Q4", "2020Q4"))

  out <- test_set_portfolio_parameters_in_callr(parameters)
  expect_equal(out$port_holdings_date, c("2019Q4", "2020Q4"))

  # test that when numeric values are in the parameters file they are properly
  # converted to characters
  parameters <-
    list(
      portfolio_name = 21L,
      investor_name = 2134L,
      peer_group = 52L,
      user_id = 2345L
    )

  out <- test_set_portfolio_parameters_in_callr(parameters)
  expect_equal(out$portfolio_name, "21")
  expect_equal(out$investor_name, "2134")
  expect_equal(out$peer_group, "52")
  expect_equal(out$user_id, "2345")
})
