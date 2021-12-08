test_that("`set_portfolio_parameters()` works as expected", {
  skip_if(nzchar(Sys.getenv("R_CMD")), "In R CMD check")

  web_functions <- new.env()
  on.exit(rm(web_functions), add = TRUE)
  source("../../0_web_functions.R", local = web_functions)

  save_yaml <- function(.params, .file = "portfolio_config.yml") {
    yaml::write_yaml(list(default = list(parameters = .params)), .file)
    invisible(.file)
  }

  cleanup <- function(file_names, object_names) {
    unlink(file_names)
    rm(list = object_names, envir = .GlobalEnv)
    invisible()
  }

  expect_global_objects <- function(expected_global_objects) {
    for (i in seq_along(expected_global_objects)) {
      expect(
        identical(
          base::get(names(expected_global_objects)[[i]], envir = .GlobalEnv, inherits = FALSE),
          expected_global_objects[[i]]
        ),
        "object with expected name and value not in global environment"
      )
    }
  }

  expect_no_global_objects <- function(expected_global_objects) {
    for (i in seq_along(expected_global_objects)) {
      expect_false(exists(names(expected_global_objects)[[i]], where = .GlobalEnv),
        info = "unexpected object in global environment"
      )
    }
  }

  test_and_cleanup <- function(parameters, expected_global_objects) {
    filename <- save_yaml(parameters)
    web_functions$set_portfolio_parameters(filename)
    expect_global_objects(expected_global_objects)
    cleanup(filename, names(expected_global_objects))
    expect_no_global_objects(expected_global_objects)
    rm("parameters", envir = parent.frame())
    rm("expected_global_objects", envir = parent.frame())
  }

  # test that standard parameter names are read in to
  # the proper object names in the global environment
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
  expected_global_objects <-
    list(
      portfolio_name = "TestPortfolio",
      investor_name = "TestInvestor",
      peer_group = "test_group",
      language_select = "DE",
      user_id = "2345",
      project_code = "GENERAL",
      port_holdings_date = "2020Q4"
    )
  test_and_cleanup(parameters, expected_global_objects)

  # test that non-standard parameter names that have been used in the past are
  # still read in and set to the proper object names in the global environment
  parameters <-
    list(
      portfolio_name_in = "TestPortfolio",
      investor_name_in = "TestInvestor"
    )
  expected_global_objects <-
    list(
      portfolio_name = "TestPortfolio",
      investor_name = "TestInvestor",
      peer_group = NULL,
      language_select = NULL,
      user_id = NULL,
      project_code = NULL,
      port_holdings_date = NULL
    )
  test_and_cleanup(parameters, expected_global_objects)

  # test that if necessary parameters are unset in the config file the
  # proper object names will be set to NULL in the global environment
  parameters <- list()
  expected_global_objects <-
    list(
      portfolio_name = NULL,
      investor_name = NULL,
      peer_group = NULL,
      language_select = NULL,
      user_id = NULL,
      project_code = NULL,
      port_holdings_date = NULL
    )
  test_and_cleanup(parameters, expected_global_objects)

  # test that when holdings_date is set to a list of values that are the same
  # value, the port_holdings_date in the global environment will be set to a
  # vector of those same multiple values
  parameters <- list(holdings_date = c("2020Q4", "2020Q4"))
  expected_global_objects <-
    list(
      portfolio_name = NULL,
      investor_name = NULL,
      peer_group = NULL,
      language_select = NULL,
      user_id = NULL,
      project_code = NULL,
      port_holdings_date = c("2020Q4", "2020Q4")
    )
  test_and_cleanup(parameters, expected_global_objects)

  # test that when holdings_date is set to a list of values that are different,
  # the port_holdings_date in the global environment will be set to a vector of
  # those same different values
  parameters <- list(holdings_date = c("2019Q4", "2020Q4"))
  expected_global_objects <-
    list(
      portfolio_name = NULL,
      investor_name = NULL,
      peer_group = NULL,
      language_select = NULL,
      user_id = NULL,
      project_code = NULL,
      port_holdings_date = c("2019Q4", "2020Q4")
    )
  test_and_cleanup(parameters, expected_global_objects)
})
