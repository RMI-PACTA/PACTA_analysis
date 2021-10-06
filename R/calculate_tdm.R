#' Calculate the transition disruption metric, based on the IPR scenario
#'
#' @param data A dataset like the `*_results_portfolio.rda` outputs of PACTA.
#' @param start_year The start year for which the TDM value should be calculated.
#' @param ... Variables to group by.
#'
#' @return A tibble with the columns specified in the `...` input, as well as:
#'   `tdm_tech`, the technology level transition disruption metric and `tdm_sec`,
#'    the sector level transition disruption metric.
#' @export
#'
#' @examples
#' pacta_results <- tibble::tibble(
#'   investor_name = rep("some_investor", 3),
#'   portfolio_name = rep("some_portfolio", 3),
#'   allocation = rep("portfolio_weight", 3),
#'   equity_market = rep("GlobalMarket", 3),
#'   scenario_geography = rep("Global", 3),
#'   ald_sector = rep("Power", 3),
#'   technology = rep("RenewablesCap", 3),
#'   year = c(2020, 2025, 2030),
#'   plan_alloc_wt_tech_prod = rep(1, 3),
#'   scen_alloc_wt_tech_prod = 1:3,
#'   plan_carsten = rep(0.5, 3)
#' )
#' pacta_results
#'
#' calculate_tdm(pacta_results, start_year = 2020)
calculate_tdm <- function(data, start_year, ...) {
  check_calculate_tdm(data, start_year)

  crucial_groups <- function() c("technology", "ald_sector")
  crucial <- c(
    "allocation",
    crucial_groups(),
    "year",
    "scen_alloc_wt_tech_prod",
    "plan_alloc_wt_tech_prod",
    "plan_carsten"
  )

  check_crucial_names(data, crucial)

  groups <- c(crucial_groups(), ...)

  data <- data %>%
    # TODO: This function will only work if the allocation method is portfolio_weight
    # ownership_weight outputs 0 for all carsten metric values, and thus we would be
    # dividing by zero otherwise...
    filter(.data$allocation == "portfolio_weight") %>%
    warn_if_has_zero_rows(
      'Filtering for "portfolio_weight" allocation, outputs 0 rows'
    )

  if (nrow(data) == 0) {
    return(empty_calculate_tdm())
  }

  technology_level_dy <- data %>%
    filter(.data$year %in% c(start_year, start_year + 5, start_year + 10)) %>%
    mutate(
      time_step = case_when(
        .data$year == start_year ~ "start_year",
        .data$year == start_year + 5 ~ "plus_five",
        .data$year == start_year + 10 ~ "plus_ten"
      )
    ) %>%
    group_by(!!!rlang::syms(groups)) %>%
    select(
      !!!rlang::syms(groups),
      .data$time_step,
      scen_alloc = "scen_alloc_wt_tech_prod",
      plan_alloc = "plan_alloc_wt_tech_prod"
    ) %>%
    tidyr::pivot_wider(
      names_from = .data$time_step,
      values_from = c("scen_alloc", "plan_alloc")
    ) %>%
    mutate(
      .numerator = .data$scen_alloc_plus_ten - .data$plan_alloc_plus_five,
      .denominator = .data$scen_alloc_plus_ten - .data$scen_alloc_start_year,
      tdm_tech = max(0, .data$.numerator / .data$.denominator) * 2
    ) %>%
    select(.data$tdm_tech, !!!rlang::syms(groups)) %>%
    ungroup()

  data %>%
    filter(.data$year == start_year) %>%
    left_join(technology_level_dy, by = groups) %>%
    group_by(!!!rlang::syms(groups)) %>%
    ungroup(.data$technology) %>%
    mutate(
      tdm_sec = .data$plan_carsten * sum(.data$tdm_tech) / sum(.data$plan_carsten),
      reference_year = start_year
    ) %>%
    ungroup() %>%
    select(!!!rlang::syms(groups), .data$tdm_tech, .data$tdm_sec, .data$reference_year)
}

check_calculate_tdm <- function(data, start_year) {
  stopifnot(is.data.frame(data), is.numeric(start_year))
}

warn_if_has_zero_rows <- function(data, message) {
  if (nrow(data) == 0L) warn(message = message, class = "has_zero_rows")
  invisible(data)
}

empty_calculate_tdm <- function() {
  tibble::tibble(
    technology = character(0),
    ald_sector = character(0),
    tdm_tech = numeric(0),
    tdm_sec = numeric(0),
    reference_year = integer(0),
  )
}
