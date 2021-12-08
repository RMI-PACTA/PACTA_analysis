test_that("`set_portfolio_parameters()` works as expected", {
  skip_if(nzchar(Sys.getenv("R_CMD")), "In R CMD check")

  source("../../0_web_functions.R")

  local_yaml <- function(.params,
                         .file = "portfolio_config.yml",
                         .local_envir = parent.frame()) {
    withr::local_file(.file = .file, .local_envir = .local_envir)
    yaml::write_yaml(list(default = list(parameters = .params)), .file)
    invisible(.file)
  }

  # test that standard parameter names are read in to
  # the proper object names in the global environment
  withr::with_environment(
    emptyenv(),
    {
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
      filename <- local_yaml(parameters)

      set_portfolio_parameters(filename)
      expect_equal(portfolio_name, "TestPortfolio")
      expect_equal(investor_name, "TestInvestor")
      expect_equal(peer_group, "test_group")
      expect_equal(language_select, "DE")
      expect_equal(user_id, "2345")
      expect_equal(project_code, "GENERAL")
      expect_equal(port_holdings_date, "2020Q4")
    }
  )

  # test that non-standard parameter names that have been used in the past are
  # still read in and set to the proper object names in the global environment
  withr::with_environment(
    emptyenv(),
    {
      parameters <- list(
        portfolio_name_in = "TestPortfolio",
        investor_name_in = "TestInvestor"
      )
      filename <- local_yaml(parameters)

      set_portfolio_parameters(filename)
      expect_equal(portfolio_name, "TestPortfolio")
      expect_equal(investor_name, "TestInvestor")
    }
  )

  # test that if necessary parameters are unset in the config file the
  # proper object names will be set to NULL in the global environment
  withr::with_environment(
    emptyenv(),
    {
      parameters <- list()
      filename <- local_yaml(parameters)

      set_portfolio_parameters(filename)
      expect_null(portfolio_name)
      expect_null(investor_name)
      expect_null(peer_group)
      expect_null(language_select)
      expect_null(user_id)
      expect_null(project_code)
      expect_null(port_holdings_date)
    }
  )

  # test that when holdings_date is set to a list of values that are the same
  # value, the port_holdings_date in the global environment will be set to a
  # vector of those same multiple values
  withr::with_environment(
    emptyenv(),
    {
      parameters <- list(holdings_date = c("2020Q4", "2020Q4"))
      filename <- local_yaml(parameters)

      set_portfolio_parameters(filename)
      expect_equal(port_holdings_date, c("2020Q4", "2020Q4"))
    }
  )

  # test that when holdings_date is set to a list of values that are different,
  # the port_holdings_date in the global environment will be set to a vector of
  # those same different values
  withr::with_environment(
    emptyenv(),
    {
      parameters <- list(holdings_date = c("2019Q4", "2020Q4"))
      filename <- local_yaml(parameters)

      set_portfolio_parameters(filename)
      expect_equal(port_holdings_date, c("2019Q4", "2020Q4"))
    }
  )
})
