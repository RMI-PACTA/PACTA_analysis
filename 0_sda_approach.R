devtools::load_all()

sda_portfolio_target <- function(
                                 market,
                                 portfolio,
                                 scenario = "B2DS",
                                 geography = "Global",
                                 ald_sector = NULL,
                                 start_year = NULL,
                                 target_year = NULL) {
  stopifnot(is.data.frame(market), is.data.frame(portfolio))
  old_market <- market
  old_portfolio <- portfolio
  market <- clean_column_names(market)
  portfolio <- clean_column_names(portfolio)
  check_names_sector_and_geography(market, portfolio, scenario, geography)
  ald_sector <- validate_sector(market, portfolio, ald_sector = ald_sector)
  message("* Using `ald_sector`: ", paste0(ald_sector, collapse = ", "), ".")
  start_year <- validate_start_year(market, portfolio, start_year)
  message("* Using `start_year`: ", start_year, ".")
  target_year <- validate_target_year_by_sector(market, target_year, ald_sector)
  message("* Using `target_year`: ", target_year, ".")
  distinct_vars <- c(get_sda_common_vars(), "scen_sec_emissions_factor", "year")
  port_to_market <- create_port_to_market(
    market = market,
    portfolio = portfolio,
    distinct_vars = distinct_vars,
    scenario = scenario,
    ald_sector = ald_sector,
    geography = geography,
    start_year = start_year,
    target_year = target_year
  )
  distance <- create_distance(
    market = market,
    portfolio = portfolio,
    scenario = scenario,
    ald_sector = ald_sector,
    geography = geography,
    start_year = start_year,
    target_year = target_year
  )
  out <- right_join(
    create_porttomarket_distance(port_to_market, distance, distinct_vars),
    portfolio,
    by = c(get_sda_common_by(), "investor_name", "portfolio_name", "year"),
    suffix = c("", "_no_sda")
  ) %>%
    select(-.data$scen_sec_emissions_factor_no_sda) %>%
    select(colnames(portfolio))


  out %>% unclean_column_names(unclean = old_market)
}

check_names_sector_and_geography <- function(market,
                                             portfolio,
                                             scenario,
                                             geography) {
  crucial <- get_sda_crucial_vars()
  check_crucial_names(market, crucial)
  check_crucial_names(portfolio, crucial)
  check_ref(market, portfolio, ref = scenario, col = "scenario")
  check_ref(market, portfolio, ref = geography, col = "scenario_geography")
  invisible(market)
}

check_ref <- function(market, portfolio, ref, col) {
  ref_has_length_1 <- identical(length(ref), 1L)
  stopifnot(ref_has_length_1)
  valid <- sort(unique(c(market[[col]], portfolio[[col]])))
  is_valid <- any(ref %in% valid)
  if (!is_valid) {
    stop(
      "Can't use ", ref, ". Must be one of:\n", paste0(valid, collapse = ", "),
      call. = FALSE
    )
  }
}

validate_sector <- function(market, portfolio, ald_sector) {
  ald_sector <- ald_sector %||% get_sectors()
  useful <- intersect(market$ald_sector, portfolio$ald_sector)
  sector_in_data <- intersect(ald_sector, useful)
  using <- abort_cant_find(sector_in_data)
  warn_unused_sector(setdiff(ald_sector, useful))
  using
}

abort_cant_find <- function(using) {
  is_found <- length(using) > 0L
  stopifnot(is_found)
  invisible(using)
}

warn_unused_sector <- function(unused) {
  if (length(unused) > 0L) {
    warning(
      "Skipping sectors not present in both `market` and `portfolio`:\n",
      paste0(unused, collapse = ", "), ".",
      call. = FALSE
    )
  }
  invisible(unused)
}

validate_start_year <- function(market, portfolio, start_year) {
  start_year <- start_year %||% START.YEAR()
  abort_null_start_year(start_year)
  abort_bad_year(market, start_year)
  start_year
}

abort_null_start_year <- function(start_year) {
  if (is.null(start_year)) {
    stop(
      "`start_year` can't be NULL.\n",
      "Did you forget `start_year` in a configuration file or as an argument?",
      call. = FALSE
    )
  }
  invisible(start_year)
}

find_years_by_sector <- function(market, sectors) {
  picked_sectors <- filter(market, .data$ald_sector %in% sectors)
  split(picked_sectors$year, picked_sectors$ald_sector) %>%
    purrr::reduce(intersect)
}

validate_target_year_by_sector <- function(market, target_year, ald_sector) {
  useful_years <- market %>% find_years_by_sector(ald_sector)
  if (is.null(target_year)) {
    return(max(useful_years))
  }
  target_year_has_length_1 <- identical(length(target_year), 1L)
  stopifnot(target_year_has_length_1)
  best_target_year <- intersect(target_year, useful_years)
  is_target_year_shared_across_sectors <- length(best_target_year) > 0L
  stopifnot(is_target_year_shared_across_sectors)
  best_target_year
}

abort_bad_year <- function(data, year) {
  year_has_length_1 <- identical(length(year), 1L)
  stopifnot(year_has_length_1, is.character(year) || is.numeric(year))
  is_year_in_data <- any(year %in% unique(data$year))
  stopifnot(is_year_in_data)
  invisible(year)
}

create_distance <- function(market,
                            portfolio,
                            scenario,
                            ald_sector,
                            geography,
                            start_year,
                            target_year) {
  distinct_vars <- dplyr::vars(!!!syms(get_sda_common_vars()), .data$CI)

  ci_port <- portfolio %>%
    pick_scenario_sector_and_geography(scenario, ald_sector, geography) %>%
    filter(as.character(.data$year) == as.character(start_year)) %>%
    filter(!is.na(.data[["plan_sec_emissions_factor"]])) %>%
    rename(CI = .data$plan_sec_emissions_factor) %>%
    distinct(!!!distinct_vars)
  ci_market <- market %>%
    filter(as.character(.data$year) == as.character(start_year)) %>%
    filter(!is.na(.data[["scen_sec_emissions_factor"]])) %>%
    rename(CI = .data$scen_sec_emissions_factor) %>%
    distinct(!!!distinct_vars)
  si <- market %>%
    filter(as.character(.data$year) == as.character(target_year)) %>%
    filter(!is.na(.data[["scen_sec_emissions_factor"]])) %>%
    rename(CI = .data$scen_sec_emissions_factor) %>%
    distinct(!!!distinct_vars) %>%
    rename(SI = .data$CI)
  cimarket_si <- inner_join(
    ci_market, si,
    by = c(get_sda_common_by(), "investor_name", "portfolio_name")
  )
  inner_join(
    cimarket_si, ci_port,
    by = get_sda_common_by(), suffix = c("_market", "_port")
  ) %>%
    mutate(D_port = .data$CI_port - .data$SI)
}
create_port_to_market <- function(market,
                                  portfolio,
                                  distinct_vars,
                                  scenario,
                                  ald_sector,
                                  geography,
                                  start_year,
                                  target_year) {
  lhs <- market %>%
    distinct(!!!syms(distinct_vars)) %>%
    filter(
      as.character(.data$year) >= as.character(start_year) &
        as.character(.data$year) <= as.character(target_year)
    ) %>%
    pick_scenario_sector_and_geography(scenario, ald_sector, geography) %>%
    select(-c(.data$investor_name, .data$portfolio_name))
  rhs <- portfolio %>%
    distinct(!!!syms(distinct_vars))
  port_to_market <- inner_join(
    lhs, rhs,
    by = c(get_sda_common_by(), "year"), suffix = c("_port", "_market")
  )
}
create_porttomarket_distance <- function(port_to_market, distance, distinct_vars) {
  inner_join(
    port_to_market, distance,
    by = c(
      get_sda_common_by(),
      "investor_name" = "investor_name_port",
      "portfolio_name" = "portfolio_name_port"
    )
  ) %>%
    mutate(
      P_market = (.data$scen_sec_emissions_factor_market - .data$SI) /
        (.data$CI_market - .data$SI),
      scen_sec_emissions_factor = (.data$D_port * 1 * .data$P_market) + .data$SI
    ) %>%
    select(!!!distinct_vars)
}

#' Default value for the `ald_sector` argument to [sda_portfolio_target()]
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' get_sectors()
get_sectors <- function() {
  c(
    "Cement",
    "Steel",
    "Power",
    "Oil&Gas",
    "Coal",
    "Aviation",
    "FossilFuels",
    "Shipping",
    "Automotive"
  )
}
get_sda_common_by <- function() {
  c(
    "allocation",
    "ald_sector",
    "scenario",
    "scenario_geography"
  )
}
get_sda_common_vars <- function() {
  c(
    get_sda_common_by(),
    "investor_name",
    "portfolio_name"
  )
}
get_sda_crucial_vars <- function() {
  c(
    get_sda_common_vars(),
    "plan_sec_emissions_factor",
    "scen_sec_emissions_factor",
    "year"
  )
}
pick_scenario_sector_and_geography <- function(data,
                                               scenario,
                                               ald_sector,
                                               geography) {
  data %>%
    filter(
      .data$scenario %in% scenario &
        .data$ald_sector %in% ald_sector &
        .data$scenario_geography %in% geography
    )
}
