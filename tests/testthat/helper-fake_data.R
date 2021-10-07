#' Minimal explicit PACTA `*_results_portfolio.rda` datasets that allow
#' overwriting values
#'
#' These functions are developer-oriented. They all call [tibble()] so
#' you can expect all the goodies that come with that.
#' * `fake_tdm_data()` fakes the `_results_portfolio.rda` output of PACTA.
#'
#' @section Params
#' The arguments are the column names of the datasets being faked. They all have
#' a default and it can be overwritten.
#'
#' @section Pros and cons
#' These functions help you to avoid duplicating test code, and help
#' the reader of your code to focus on the one thing you want to test, instead
#' of burring that thing in the much longer code you need to create a fake
#' object from scratch.
#'
#' But `fake_*()` functions hide the explicit content. If the reader of your
#' code wants to inspect the data being tested, they need to jump to the
#' function definition or call them interactively.
#'
#' @return A data.frame
#'
#' @examples
#' fake_tdm_data()
#'
#' fake_tdm_data(allocation = c("portfolio_weight", "ownership_weight"))
#'
#' fake_tdm_data(new = "abc")
#'
#' # Support for trailing commas
#' fake_tdm_data(allocation = "portfolio_weight", )
#' @noRd
fake_tdm_data <- function(investor_name = NULL,
                          portfolio_name = NULL,
                          allocation = NULL,
                          equity_market = NULL,
                          scenario_geography = NULL,
                          ald_sector = NULL,
                          technology = NULL,
                          year = NULL,
                          plan_alloc_wt_tech_prod = NULL,
                          scen_alloc_wt_tech_prod = NULL,
                          plan_carsten = NULL,
                          ...) {
  tibble(
    investor_name = investor_name %||% "some_investor",
    portfolio_name = portfolio_name %||% "some_portfolio",
    allocation = allocation %||% "portfolio_weight",
    equity_market = equity_market %||% "GlobalMarket",
    scenario_geography = scenario_geography %||% "Global",
    ald_sector = ald_sector %||% "Power",
    technology = technology %||% "RenewablesCap",
    year = year %||% 2020,
    plan_alloc_wt_tech_prod = plan_alloc_wt_tech_prod %||% 1,
    scen_alloc_wt_tech_prod = scen_alloc_wt_tech_prod %||% 1,
    plan_carsten = plan_carsten %||% 0.5,
    ...
  )
}
