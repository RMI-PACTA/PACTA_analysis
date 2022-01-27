#' Calculate and add the transition disruption metric to a dataset
#'
#' This function takes a typical PACTA output, and uses it to calculate the
#' transition disruption metric.
#'
#' @param data A data.frame (or tibble), with a format similar to the
#'   `*_results_portfolio.rda` outputs of PACTA.
#' @param t0 The start year, from which the TDM value should be calculated. This
#'   value should coincide with the earliest year in the PACTA results dataset.
#' @param delta_t1 The number of years into the future for which there is production
#'   data. The default value is 5 years since this is often the limit of the
#'   forward-looking data.
#' @param delta_t2 The number of years into the future against which you want to
#'   calculate disruption.
#' @param additional_groups Character vector. The names of columns to group by,
#'   in addition to these ones (which are always used):
#'
#'   `r toString(crucial_tdm_groups())`
#' @param scenarios Character vector. The names of scenarios for which TDM
#'   should be calculated.
#'
#' @return A tibble with the columns specified in the `additional_groups` input
#'   as well as `tdm_technology_value`: the technology level transition
#'   disruption metric, `tdm_sector_value`: the sector level transition
#'   disruption metric, `tdm_portfolio_value`: the portfolio level transition
#'   disruption metric, `tdm_metric`: a flag that indicates what each
#'   `tdm_*_value` is being calculated against (e.g. "portfolio" or "scenario"),
#'   as well as `tdm_t0`, `tdm_delta_t1` and `tdm_delta_t2`: corresponding to
#'   the input arguments.
#' @export
#'
#' @examples
#' library(tibble)
#'
#' pacta_results <- tibble(
#'   investor_name = rep("some_investor", 3),
#'   portfolio_name = rep("some_portfolio", 3),
#'   scenario = rep("IPR FPS 2021", 3),
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
#' calculate_tdm(pacta_results, t0 = 2020)
calculate_tdm <- function(data,
                          t0,
                          delta_t1 = 5,
                          delta_t2 = 10,
                          additional_groups = NULL,
                          scenarios = "IPR FPS 2021") {
  stopifnot(
    is.data.frame(data),
    is.numeric(t0),
    is.numeric(delta_t1),
    is.numeric(delta_t2)
  )

  if (!is.null(additional_groups)) stopifnot(is.character(additional_groups))

  groups <- union(crucial_tdm_groups(), additional_groups)

  check_calculate_tdm(data, t0, delta_t1, delta_t2, groups)

  filtered_data <- dplyr::filter(
    data,
    .data$allocation == "portfolio_weight"
  )

  if (nrow(filtered_data) == 0) {
    # NOTE: This function will only work if the allocation method is
    # portfolio_weight. ownership_weight outputs 0 for all `carsten_metric` values,
    # which would later result in a divide by 0 issue.
    warning_message <- 'Filtering for "portfolio_weight" allocation, outputs 0 rows'
    return(warn_zero_rows(tdm_prototype(), warning_message))
  }

  filtered_data <- dplyr::filter(
    filtered_data,
    .data$scenario %in% scenarios
  )

  if (nrow(filtered_data) == 0) {
    warning_message <- "Filtering for `scenarios`, outputs 0 rows"
    return(warn_zero_rows(tdm_prototype(), warning_message))
  }

  data_with_monotonic_factors <- add_monotonic_factor(filtered_data, t0, delta_t1, delta_t2, groups)

  initial_year_data <- dplyr::filter(data_with_monotonic_factors, .data$year == t0)

  preformatted_data <- pre_format_data(data_with_monotonic_factors, t0, delta_t1, delta_t2, groups)

  data_with_tdm <- add_tdm(preformatted_data, delta_t1, delta_t2, groups)

  formatted_data_with_tdm <- left_join(
    initial_year_data,
    data_with_tdm,
    by = groups
  )

  formatted_data_with_tdm %>%
    mutate(
      tdm_t0 = .env$t0,
      tdm_delta_t1 = .env$delta_t1,
      tdm_delta_t2 = .env$delta_t2
    ) %>%
    add_aggregate_tdm(groups) %>%
    select(names(tdm_prototype()), all_of(groups))
}

warn_zero_rows <- function(data, message) {
  warn(message, class = "has_zero_rows")

  invisible(data)
}

add_time_step <- function(data, t0, delta_t1, delta_t2) {
  data %>%
    mutate(
      time_step = case_when(
        .data$year == .env$t0 ~ "t0",
        .data$year == .env$t0 + .env$delta_t1 ~ "plus_delta_t1",
        .data$year == .env$t0 + .env$delta_t2 ~ "plus_delta_t2"
      )
    )
}

add_monotonic_factor <- function(data, t0, delta_t1, delta_t2, groups) {
  data <- data %>%
    group_by(!!!rlang::syms(groups)) %>%
    mutate(
      end_year = max(.data$year),
      end_year_is_t0_delta_t2 = .data$end_year == t0 + delta_t2
    )

  if (all(data$end_year_is_t0_delta_t2)) {
    monotonic_factors <-
      data %>%
      ungroup() %>%
      select(.env$groups) %>%
      distinct() %>%
      mutate(monotonic_factor = 1L)
  } else {
    monotonic_factors <- data %>%
      add_time_step(t0, delta_t1, delta_t2) %>%
      mutate(time_step = case_when(
        .data$year == .data$end_year ~ "end_year",
        TRUE ~ .data$time_step
      )) %>%
      select(
        -c(
          .data$year,
          .data$end_year,
          .data$plan_carsten,
          .data$plan_alloc_wt_tech_prod,
          .data$end_year_is_t0_delta_t2
        )
      ) %>%
      pivot_wider(
        names_from = .data$time_step,
        values_from = .data$scen_alloc_wt_tech_prod
      ) %>%
      mutate(
        .increasing_overall = .data$end_year > .data$t0,
        .increasing_in_interval = .data$plus_delta_t2 > .data$t0,
        .is_monotonic = .data$.increasing_in_interval & .data$.increasing_overall,
        monotonic_factor = dplyr::if_else(.data$.is_monotonic, 1, -1),
        .increasing_overall = NULL,
        .increasing_in_interval = NULL,
        .is_monotonic = NULL,
        t0 = NULL,
        plus_delta_t1 = NULL,
        plus_delta_t2 = NULL,
        end_year = NULL
      ) %>%
      ungroup()
  }

  left_join(data, monotonic_factors, by = groups)
}

add_tdm <- function(data, delta_t1, delta_t2, groups) {
  data %>%
    group_by(!!!rlang::syms(groups)) %>%
    mutate(
      .numerator_portfolio = .data$scenario_production_plus_delta_t2 - .data$portfolio_production_plus_delta_t1,
      .numerator_scenario = .data$scenario_production_plus_delta_t2 - .data$scenario_production_plus_delta_t1,
      .denominator = .data$scenario_production_plus_delta_t2 - .data$scenario_production_t0,
      .time_factor = (delta_t2 / (delta_t2 - delta_t1)),
      tdm_technology_value_portfolio = ifelse(
        .data$.denominator == 0,
        0,
        max(0, (.data$.numerator_portfolio / .data$.denominator) * .data$monotonic_factor) * .data$.time_factor
      ),
      tdm_technology_value_scenario = ifelse(
        .data$.denominator == 0,
        0,
        max(0, (.data$.numerator_scenario / .data$.denominator) * .data$monotonic_factor) * .data$.time_factor
      ),
      .numerator_portfolio = NULL,
      .numerator_scenario = NULL,
      .denominator = NULL,
      .time_factor = NULL
    ) %>%
    select(.data$tdm_technology_value_portfolio, .data$tdm_technology_value_scenario, all_of(groups)) %>%
    distinct() %>%
    pivot_longer(
      cols = tidyr::starts_with("tdm_technology_value"),
      names_to = "tdm_metric",
      names_prefix = "tdm_technology_value_",
      values_to = "tdm_technology_value"
    ) %>%
    ungroup()
}

add_aggregate_tdm <- function(data, groups) {
  groups <- c(groups, "tdm_metric")

  data %>%
    group_by(!!!rlang::syms(groups)) %>%
    arrange(.data$year) %>%
    mutate(initial_carsten = dplyr::first(.data$plan_carsten)) %>%
    ungroup(.data$technology) %>%
    mutate(
      tdm_sector_value = sum(.data$initial_carsten * .data$tdm_technology_value) / sum(.data$initial_carsten)
    ) %>%
    ungroup(.data$ald_sector) %>%
    mutate(
      tdm_portfolio_value = sum(.data$initial_carsten * .data$tdm_technology_value) / sum(.data$initial_carsten)
    ) %>%
    mutate(initial_carsten = NULL) %>%
    ungroup()
}

pre_format_data <- function(data, t0, delta_t1, delta_t2, groups) {
  data %>%
    dplyr::filter(.data$year %in% c(t0, t0 + delta_t1, t0 + delta_t2)) %>%
    group_by(!!!rlang::syms(groups)) %>%
    add_time_step(t0, delta_t1, delta_t2) %>%
    select(
      !!!rlang::syms(groups),
      .data$time_step,
      .data$monotonic_factor,
      scenario_production = .data$scen_alloc_wt_tech_prod,
      portfolio_production = .data$plan_alloc_wt_tech_prod,
    ) %>%
    pivot_wider(
      names_from = .data$time_step,
      values_from = all_of(c("scenario_production", "portfolio_production"))
    ) %>%
    ungroup()
}

tdm_prototype <- function() {
  tibble(
    technology = character(0),
    ald_sector = character(0),
    tdm_metric = character(0),
    tdm_technology_value = numeric(0),
    tdm_sector_value = numeric(0),
    tdm_portfolio_value = numeric(0),
    tdm_t0 = integer(0),
    tdm_delta_t1 = integer(0),
    tdm_delta_t2 = integer(0)
  )
}

check_calculate_tdm <- function(data, t0, delta_t1, delta_t2, groups) {
  check_crucial_names(data, crucial_columns())

  check_crucial_years(data, t0, delta_t1, delta_t2, groups)

  check_unique_by_year_and_groups(data, groups)

  invisible(data)
}

check_unique_by_year_and_groups <- function(data, groups) {
  req_unique_columns <- c(
    "scen_alloc_wt_tech_prod",
    "plan_alloc_wt_tech_prod",
    "plan_carsten"
  )

  data <- data %>%
    group_by(!!!rlang::syms(c("year", groups))) %>%
    dplyr::summarize(rows_by_year_and_group = dplyr::n(), .groups = "drop") %>%
    mutate(rows_are_unique = .data$rows_by_year_and_group == 1)

  ok <- all(data$rows_are_unique)
  if (!ok) {
    rlang::abort(
      message = c(
        "Data must be unique by year and groups for the following columns",
        x = glue::glue_collapse(
          req_unique_columns,
          sep = ",",
          width = 50,
          last = "and"
        ),
        i = "Are you sure you have included the correct groupings?"
      ),
      class = "multiple_values_per_year"
    )
  }

  invisible(data)
}

check_crucial_years <- function(data, t0, delta_t1, delta_t2, groups) {
  crucial_years <- tibble::tibble(crucial_years = c(t0, t0 + delta_t1, t0 + delta_t2))

  missing_crucial_years <- crucial_years %>%
    left_join(data, by = c(crucial_years = "year")) %>%
    group_by(!!!rlang::syms(groups)) %>%
    dplyr::summarize(missing_data = sum(is.na(.data$plan_alloc_wt_tech_prod)), .groups = "drop")

  ok <- all(missing_crucial_years$missing_data == 0)

  if (!ok) {
    rlang::abort(
      message = c(
        "Data must contain all crucial years, by group:",
        x = glue::glue_collapse(
          crucial_years,
          sep = ",",
          width = 50,
          last = "and"
        ),
        i = "Are all crucial years present in input data?"
      ),
      class = "missing_crucial_years"
    )
  }
}

crucial_tdm_groups <- function() {
  c("technology", "ald_sector")
}

crucial_columns <- function() {
  c(
    crucial_tdm_groups(),
    "allocation",
    "year",
    "scen_alloc_wt_tech_prod",
    "plan_alloc_wt_tech_prod",
    "plan_carsten"
  )
}
