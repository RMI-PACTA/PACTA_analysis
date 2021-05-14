calc_sda_target <- function(portfolio,
                            scen_emission_fctrs,
                            average_sector_emissions_factor_from_ALD = NULL,
                            start_year = NULL) {

  suppressMessages(requireNamespace("dplyr"))
  suppressMessages(requireNamespace("tidyr"))
  suppressMessages(requireNamespace("zoo"))
  suppressMessages(requireNamespace("rlang"))
  suppressMessages(requireNamespace("cli"))

  # set start_year to the first year of data in the portfolio unless set explicitly
  if (is.null(start_year)) start_year <- min(portfolio$year, na.rm = TRUE)

  # check that scen_emission_fctrs has data for 2050
  errors <-
    scen_emission_fctrs %>%
    dplyr::group_by(scenario, ald_sector) %>%
    dplyr::summarise(n = sum(year == 2050) == 1, .groups = "drop") %>%
    dplyr::filter(!n)

  if (nrow(errors) > 0L) {
    msgs <- apply(errors, 1, function(x) {
      paste0("scenario {.val ", x[1], "} does not have 2050 data for sector {.val ", x[2], "}", collapse = " ")
    })
    msgs <- setNames(msgs, rep("x", length(msgs)))

    rlang::abort(cli::format_error(c(
      "all scenario/sector pairs must have data for year 2050",
      msgs
    )))
  }

  # prepare scenario data
  scen_emission_fctrs <-
    scen_emission_fctrs %>%
    # all operations here should be done per scenario and sector
    dplyr::group_by(scenario, ald_sector) %>%
    # interpolate scenario_emission_factors to cover every year from its first year to 2050
    tidyr::complete(year = tidyr::full_seq(.data$year, 1L)) %>%
    dplyr::filter(year <= 2050L) %>%
    dplyr::mutate(value = zoo::na.approx(.data$value, .data$year, na.rm = FALSE)) %>%
    # start at same year as portfolio data (or explicitly set start_year)
    dplyr::filter(year >= .env$start_year) %>%
    # calculate values needed for SDA target formula
    dplyr::rename(scenario_emissions_target = .data$value) %>%
    dplyr::mutate(si_first = dplyr::first(.data$scenario_emissions_target, order_by = .data$year)) %>%
    dplyr::mutate(si_last = dplyr::last(.data$scenario_emissions_target, order_by = .data$year)) %>%
    dplyr::ungroup()

  # merge scenario data with portfolio data and calculate SDA targets
  portfolio %>%
    # merge with portfolio data
    dplyr::left_join(scen_emission_fctrs, by = c("scenario", "ald_sector", "year")) %>%
    # calculate CI for SDA target formula
    dplyr::group_by(investor_name, portfolio_name, scenario, allocation,
                    equity_market, scenario_geography, ald_sector) %>%
    #
    #
    #
    # is `plan_sec_emissions_factor` weighted by technology (production weighted average)!?!?
    #
    #
    #
    dplyr::mutate(ci = dplyr::first(.data$plan_sec_emissions_factor, order_by = .data$year)) %>%
    dplyr::ungroup() %>%
    #
    #
    #
    # apply market adjustment to si_last (and maybe si_first?)
    # * need to get "average sector emissions factor from full ALD" to set starting sda_sec_emissions_factor value
    # * hypothetically then the end point of plan_sec_emissions_factor and sda_sec_emissions_factor would converge
    # dplyr::mutate(si_last = do market adjustment of scenario target: see market adjustment info in docs) %>%
    #
    #
    #
    # calculate more values needed for SDA target formula
    dplyr::mutate(dist = .data$ci - .data$si_last) %>%
    dplyr::mutate(p = (.data$scenario_emissions_target - .data$si_last) / (.data$si_first - .data$si_last)) %>%
    # calculate SDA target
    dplyr::mutate(sda_sec_emissions_factor = (.data$dist * .data$p * 1) + .data$si_last) %>%
    # return the final calculated vector
    dplyr::pull(sda_sec_emissions_factor)
}
