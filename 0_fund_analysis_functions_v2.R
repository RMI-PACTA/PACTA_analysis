library(tidyverse)
library(janitor)
library(assertr)
library(fs)
library(glue)
library(usethis)

apply_traffic_light_system <- function(
  .data, 
  traffic_lights = traffic_lights, 
  indicator_list, 
  group_vars = "fund_isin"
){
  
  
  technology_exposure_power <- results %>% 
    filter(ald_sector == "power") %>% 
    summarise_technology_share(names_prefix = "technology_exposure") %>% 
    add_nice_names(
      values_from = "technology_exposure", 
      names_from = "technology", 
      names_prefix = "technology_share"
    )
  
  
  traffic_light_data_indicator <- input_data_indicator %>% left_join(traffic_light_data, by = "indicator_name")
  
  .data <- .data %>% 
    mutate(
      color = case_when(
        traffic_light_green > traffic_light_yellow & value >= traffic_light_green ~ "green",
        traffic_light_green > traffic_light_yellow & value >= traffic_light_yellow ~ "yellow",
        traffic_light_green < traffic_light_yellow & value <= traffic_light_green ~ "green",
        traffic_light_green < traffic_light_yellow & value <= traffic_light_yellow ~ "yellow",
        is.na(value) ~ "",
        TRUE ~ "red"
      )
    ) 
  traffic_light_data_indicator <- traffic_light_data_indicator %>% 
    pivot_wider(
      id_cols = all_of(group_vars),
      names_from = indicator_name,
      names_prefix = "traffic_light_",
      values_from = color
    )
  
}

add_sector_weights <- function() {
  tibble::tribble(
    ~ald_sector,                       ~technology, ~sector_weight, ~technology_weight,
    "power",                         "coalcap",           0.22,               0.25,
    "power",                          "gascap",           0.22,               0.06,
    "power",                        "hydrocap",           0.22,               0.09,
    "power",                      "nuclearcap",           0.22,               0.11,
    "power",                          "oilcap",           0.22,               0.02,
    "power",                   "renewablescap",           0.22,               0.46,
    "automotive",                        "electric",           0.09,               0.44,
    "automotive",                          "hybrid",           0.09,               0.17,
    "automotive",                             "ice",           0.09,               0.38,
    "oil&gas",                             "gas",           0.35,               0.34,
    "oil&gas",                             "oil",           0.35,               0.66,
    "coal",                            "coal",           0.23,                 NA,
    "aviation",                         "freight",           0.01,                 NA,
    "aviation",                             "mix",           0.01,                 NA,
    "aviation",                       "passenger",           0.01,                 NA,
    "cement",                        "grinding",           0.05,                 NA,
    "cement",             "integrated facility",           0.05,                 NA,
    "shipping",                               "a",           0.02,                 NA,
    "shipping",                               "b",           0.02,                 NA,
    "shipping",                               "c",           0.02,                 NA,
    "shipping",                               "d",           0.02,                 NA,
    "shipping",                               "e",           0.02,                 NA,
    "shipping",                               "f",           0.02,                 NA,
    "shipping",                               "g",           0.02,                 NA,
    "steel",         "ac-electric arc furnace",           0.03,                 NA,
    "steel",                   "blast furnace",           0.03,                 NA,
    "steel",                        "bof shop",           0.03,                 NA,
    "steel",                    "coking plant",           0.03,                 NA,
    "steel",         "dc-electric arc furnace",           0.03,                 NA,
    "steel", "direct/smelting reduction plant",           0.03,                 NA,
    "steel",               "open hearth plant",           0.03,                 NA,
    "steel",               "pelletizing plant",           0.03,                 NA,
    "steel",                 "sintering plant",           0.03,                 NA
  )
}


influencemap_weighting_methodology <- function(
  .data,
  alignment_from = "trajectory_alignment",
  ...
) {
  # crucial names
  crucial_names <- c(
    "investor_name", 
    "portfolio_name", 
    "asset_type", 
    "ald_sector", 
    "scenario", 
    "scenario_geography", 
    "allocation",
    "asset_type_exposure",
    "asset_type_sector_exposure"
  )
  # check inputs 
  r2dii.utils::check_crucial_names(.data, crucial_names) 
  # first join technology and sector weights to pacta results file 
  .data <- .data %>%
    dplyr::inner_join(
      add_sector_weights(), 
      by = c("ald_sector", "technology")
    )
  # first reweight alignment to the sector level based on the technology importance 
  .data <- .data %>%
    group_by(
      .data$investor_name, 
      .data$portfolio_name, 
      .data$asset_type, 
      .data$ald_sector, 
      .data$scenario, 
      .data$scenario_geography, 
      .data$allocation
    ) %>%
    mutate(
      alignment_sector = if_else(
        !is.na(.data$technology_weight), 
        stats::weighted.mean(.data[[alignment_from]], .data$technology_weight * .data$plan_alloc_wt_tech_prod, ...),
        .data[[alignment_from]]
      )
    )
  # then weight up to asset_type level 
  .data <- .data %>%
    group_by(
      .data$investor_name, 
      .data$portfolio_name, 
      .data$asset_type,
      .data$asset_type_exposure,
      .data$scenario, 
      .data$scenario_geography, 
      .data$allocation
    ) %>%
    summarise(alignment_asset_type = stats::weighted.mean(.data$alignment_sector, .data$sector_weight * .data$asset_type_sector_exposure, ...))
  # calculate portfolio level results 
  .data <- .data %>%
    group_by(
      .data$investor_name, 
      .data$portfolio_name,
      .data$scenario, 
      .data$scenario_geography, 
      .data$allocation
    ) %>%
    summarise(alignment_portfolio = stats::weighted.mean(.data$alignment_asset_type, .data$asset_type_exposure, ...))
  # alignment source 
  .data %>% 
    mutate(alignment_metric = alignment_from) %>% 
    ungroup()
}

summarise_technology_share <- function(
  .data, 
  emission_factors_from = "plan_emission_factor", 
  production_from = "plan_alloc_wt_tech_prod", 
  technology_share_to = "technology_exposure"
) {
  # define crucial names
  crucial_names <- c(
    "investor_name", 
    "portfolio_name", 
    "asset_type", 
    "ald_sector",
    "technology",
    "asset_type_exposure", 
    emission_factors_from,
    production_from
  )
  # check crucial names
  r2dii.utils::check_crucial_names(.data,  crucial_names)
  # calculate sector technology exposure 
  .data <- .data %>% 
    dplyr::filter(!is.nan(.data[[emission_factors_from]])) %>% 
    summarise_group_share(
      id_cols = c("investor_name", "portfolio_name", "asset_type", "asset_type_exposure"), 
      numerator_group = "technology", 
      denominator_group = "ald_sector", 
      values_from = production_from, 
      values_to = "technology_exposure", 
      na.rm = TRUE
    )
  # then roll-up to portfolio, sector-level 
  .data %>% 
    summarise_group_weight(
      id_cols = c("investor_name", "portfolio_name", "ald_sector", "technology"), 
      weights_from = "asset_type_exposure", 
      values_from = "technology_exposure", 
      values_to = technology_share_to
    )
}


summarise_sector_production_weighted_emission_factor <- function(
  .data, 
  emission_factors_from = "plan_emission_factor", 
  emission_factors_to = "emission_factor",
  production_from = "plan_alloc_wt_tech_prod"
) {
  # define crucial names
  crucial_names <- c(
    "investor_name", 
    "portfolio_name", 
    "asset_type", 
    "ald_sector",
    "technology",
    "asset_type_exposure",
    emission_factor_from,
    production_from
  )
  # check crucial names
  r2dii.utils::check_crucial_names(.data, crucial_names)
  # calculate sector technology exposure 
  .data <- .data %>% 
    dplyr::filter(
      !is.nan(.data[[emission_factor_from]]),
      !is.nan(.data[[production_from]])
    ) %>% 
    calculate_group_share(
      id_cols = c("investor_name", "portfolio_name", "asset_type"), 
      numerator_group = "technology", 
      denominator_group = "ald_sector", 
      values_from = production_from, 
      values_to = "technology_exposure", 
      na.rm = TRUE
    )
  # summarise by technology exposure 
  .data <- .data %>% 
    summarise_group_weight(
      id_cols = c("investor_name", "portfolio_name", "asset_type", "ald_sector", "asset_type_exposure"), 
      weights_from = "technology_exposure", 
      values_from = emission_factor_from, 
      values_to = emission_factor_to
    )
  # then roll-up to portfolio, sector-level 
  .data %>% 
    summarise_group_weight(
      id_cols = c("investor_name", "portfolio_name", "ald_sector"), 
      weights_from = "asset_type_exposure", 
      values_from = emission_factor_to
    )
}

check_top_groups_parameters <- function(
  n, 
  id_cols,
  values_from,
  numerator_group, 
  denominator_group
) {
  # interger checks 
  stopifnot(is.numeric(n))
  # character checks 
  stopifnot(is.character(id_cols))
  stopifnot(is.character(values_from))
  stopifnot(is.character(numerator_group))
  stopifnot(is.character(denominator_group))
}

summarise_top_groups <- function(
  .data, 
  n = 5, 
  id_cols = "investor_name",
  values_from = "value_usd",
  values_to = "group_share",
  numerator_group = "company_name", 
  denominator_group = "portfolio_name", 
  ...
) {
  # check parameters 
  check_top_groups_parameters(  
    n = n, 
    id_cols = id_cols,
    values_from = values_from,
    numerator_group = numerator_group, 
    denominator_group = denominator_group
  )
  # group_cols 
  group_vars <- unique(c(id_cols, denominator_group))
  # summarise group share
  .data <- .data %>% 
    filter(!is.na(.data[[numerator_group]])) %>% 
    summarise_group_share(
      id_cols = id_cols,
      numerator_group = numerator_group, 
      denominator_group = denominator_group,
      values_from = values_from,
      values_to = values_to,
      ...
    )
  # take top values 
  .data %>% 
    dplyr::group_by(!!!rlang::syms(group_vars)) %>% 
    dplyr::slice_max(.data[[values_to]], n = n, with_ties = FALSE) %>% 
    dplyr::mutate(rank = seq_along(.data[[numerator_group]])) %>% 
    dplyr::ungroup()
}

pivot_wider_names_and_values <- function(
  .data,
  name_prefix = "top",
  id_cols = "investor_name",
  values_from = "value_usd",
  numerator_group = "company_name", 
  denominator_group = "portfolio_name",
  values_fill = NA_integer_, 
  names_fill = NA_character_
) {
  groups_from <- c(id_cols, denominator_group)
  # add sequence 
  .data <- .data %>% 
    group_by(!!!rlang::syms(groups_from)) %>% 
    mutate(names = seq_along(.data[[numerator_group]]))
  # pivot values 
  values <- .data %>% 
    distinct(
      !!!rlang::syms(groups_from), 
      .data$names,
      .data$values
    ) %>% 
    pivot_wider_and_clean_names(
      values_from = "values", 
      names_from = "names", 
      names_prefix = paste("share", name_prefix, sep = "_"),
      values_fill = values_fill,
      digits = 2
    )
  # pivot names
  names <- .data %>% 
    distinct(
      !!!rlang::syms(groups_from), 
      .data$names,
      .data[[numerator_group]]
    ) %>% 
    pivot_wider_and_clean_names(
      values_from = numerator_group, 
      names_from = "names", 
      names_prefix = paste("name", name_prefix, sep = "_"),
      values_fill = names_fill
    )
  # join names and values 
  values %>% 
    full_join(
      names, 
      by = groups_from
    )
}

check_portfolio_data <- function(
  data, 
  crucial_names, 
  market_value_from
) {
  # introduce input 
  data <- data %>% 
    ungroup()
  # check crucial names 
  data <- data %>% 
    r2dii.utils::check_crucial_names(.data, crucial_names)
  # check market value class
  data <- data %>% 
    assertr::verify(class(.data[[market_value_from]]) == "numeric")
  # check for negative values 
  data %>% 
    assertr::assert(
      within_bounds(0,Inf),
      .data[[market_value_from]], 
      error_fun = just_warn
    )
}

calculate_sensentive_exposures <- function(
  .data, 
  id_cols = c("investor_name", "portfolio_name"),
  market_value_from = "value_usd",
  ...
) {
  # define crucial names 
  crucial_names <- c(id_cols, market_value_from, "isin")
  r2dii.utils::check_crucial_names(.data, crucial_names)
  # load oekom data 
  sensentive_exposures <- read_rds(path("data", "sensentive_sector_exposure", ext = "rds"))
  # calculate the total portfolio value in each isin 
  .data <- .data %>% 
    group_by(
      !!!rlang::syms(id_cols), 
      .data$isin
    ) %>% 
    summarise(market_value = sum(.data[[market_value_from]], ...))
  # join with sensentive sector data 
  .data <- .data %>% 
    left_join(
      sensentive_exposures, 
      by = "isin"
    ) %>% # and clean na.names 
    mutate(sensitive_sector = if_else(is.na(.data$sensitive_sector), "other", .data$sensitive_sector))
  # scaling things to avoid increasing the portfolio's market value size 
  .data %>% 
    group_by(
      !!!rlang::syms(id_cols),
      .data$isin
    ) %>% 
    mutate(adjusted_market_value = .data$market_value / n()) %>% 
    ungroup()
}

load_esg_raw_data <- function() {
  # define path
  folder_path <- path("data", "iss_esg_data", "company_exposures")
  # list files 
  files <- list.files(folder_path)
  # check file count 
  stopifnot(n_distinct(files) == 11)
  # load and map files 
  data <- map_df(
    files, 
    function(x) {
      readxl::read_xlsx(path(folder_path, x)) %>% 
        mutate(source = x)
    }
  )
  # clean input data 
  data <- data %>% 
    clean_names(case = "snake") %>% 
    filter(str_detect(isin, "[[:upper:]]{2}[[:alnum:]]{10}"))
  # extract sector 
  data <- data %>% 
    mutate(
      sensitive_sector = str_remove(source, "2DII - "),
      sensitive_sector = str_remove(sensitive_sector, "[[:punct:]][[:digit:]]{8}[[:punct:]]xlsx$"),
      sensitive_sector = snakecase::to_any_case(sensitive_sector, case = "snake"),
      sensitive_sector = str_remove(sensitive_sector, "_issuers$")
    )
  # add current data 
  data <- data %>% 
    mutate(last_updated = Sys.Date())
  # make sure no files are dropped
  stopifnot(n_distinct(data$source) == 11)
  # save output
  data %>% 
    write_rds(path("data", "sensentive_sector_exposure", ext = "rds"))
  # return data
  return(data)
}

check_group_weight_parameters <- function(
  data,
  id_cols,
  weights_from,
  values_from,
  values_to = NULL
) {
  # check parameters 
  stopifnot(is.character(id_cols))
  stopifnot(is.character(weights_from))
  stopifnot(is.character(values_from))
  stopifnot(is.character(values_to) | is.null(values_to))
  # check class 
  data %>% 
    assertr::verify(class(.data[[weights_from]]) == "numeric") %>% 
    assertr::verify(class(.data[[values_from]]) == "numeric")
}


summarise_group_weight <- function(
  .data, 
  id_cols,
  weights_from,
  values_from,
  values_to = NULL, 
  ...
) {
  # check parameters 
  .data <- .data %>% 
    check_group_weight_parameters(
      id_cols,
      weights_from,
      values_from,
      values_to
    )
  # weight value 
  .data <- .data %>% 
    dplyr::group_by(!!!rlang::syms(id_cols)) %>% 
    dplyr::summarise(weighted_value := stats::weighted.mean(.data[[values_from]], .data[[weights_from]])) %>% 
    dplyr::ungroup()
  # rename output
  if (!is.null(values_to)) {
    .data %>% 
      plyr::rename(c("weighted_value" = values_to))
  } else if (is.null(values_to)) {
    .data %>% 
      plyr::rename(c("weighted_value" = values_from))
  }
}

calculate_technology_alignment <- function(
  .data, 
  start_year = 2019, 
  time_horizon = 5, 
  id_cols = c("investor_name", "portfolio_name", "asset_type", "ald_sector", "technology"),
  plan_tech_prod_from = "plan_alloc_wt_tech_prod", 
  scen_tech_prod_from = "scen_alloc_wt_tech_prod"
) {
  .data %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(between(.data$year, start_year, start_year + time_horizon)) %>% 
    dplyr::group_by(!!!rlang::syms(id_cols)) %>% 
    dplyr::arrange(.data$year, .by_group = TRUE) %>% 
    dplyr::mutate(
      plan_technology_build_out = dplyr::last(.data[[plan_tech_prod_from]]) - dplyr::first(.data[[plan_tech_prod_from]]),
      scen_technology_build_out = dplyr::last(.data[[scen_tech_prod_from]]) - dplyr::first(.data[[scen_tech_prod_from]]), 
      tech_build_out_alignment = .data$plan_technology_build_out / .data$scen_technology_build_out
    ) %>% 
    dplyr::ungroup()
}

calculate_group_share <- function(
  .data,
  id_cols = NULL,
  numerator_group = "technology", 
  denominator_group = "ald_sector",
  values_from = "plan_alloc_wt_tech_prod",
  values_to = NULL,
  ...
) {
  # ungroup data 
  .data <- .data %>% 
    ungroup()
  # check parameters
  check_group_share_parameters(
    .data = .data,
    id_cols = id_cols,
    numerator_group = numerator_group, 
    denominator_group = denominator_group,
    values_from = values_from,
    values_to = values_to
  )
  # calculate exposure 
  .data <- .data %>%
    dplyr::group_by(
      !!!rlang::syms(id_cols),
      .data[[numerator_group]],
      .data[[denominator_group]]
    ) %>% 
    dplyr::mutate(group_share = sum(.data[[values_from]], ...)) %>% 
    group_by(
      !!!rlang::syms(id_cols),
      .data[[denominator_group]]
    ) %>% 
    dplyr::mutate(group_share = .data$group_share / sum(.data$group_share, ...)) %>% 
    dplyr::ungroup()
  # rename output
  if (!is.null(values_to)) {
    .data %>% 
      plyr::rename(c("group_share" = values_to))
  } else if (is.null(values_to)) {
    return(.data)
  }
}

calculate_build_out_alignment <- function(
  .data,
  start_year = 2019
) {
  # define things
  high_carbon_technologies <- c("oil","gas","coal","coalcap","gascap","ice", "oilcap")
  other_sectors <- c("shipping","steel","aviation","cement")
  # the mega 
  .data %>%
    group_by(
      .data$investor_name, 
      .data$portfolio_name,
      .data$asset_type, 
      .data$scenario, 
      .data$scenario_geography, 
      .data$ald_sector, 
      .data$technology
    ) %>% 
    mutate(reference_value = sum(if_else(year == start_year,plan_alloc_wt_tech_prod,0))) %>% 
    mutate(
      build_out_deviation_tech = case_when(
        !technology %in% high_carbon_technologies & (scen_alloc_wt_tech_prod - reference_value) == 0 & plan_alloc_wt_tech_prod > 0 ~ 1,
        !technology %in% high_carbon_technologies ~ (plan_alloc_wt_tech_prod - scen_alloc_wt_tech_prod) / (scen_alloc_wt_tech_prod - reference_value),
        technology %in% high_carbon_technologies & (scen_alloc_wt_tech_prod - reference_value) == 0 & (plan_alloc_wt_tech_prod > reference_value) ~ 1,
        technology %in% high_carbon_technologies & (scen_alloc_wt_tech_prod - reference_value) == 0 & (plan_alloc_wt_tech_prod < reference_value) ~ -1,
        technology %in% high_carbon_technologies & plan_alloc_wt_tech_prod > scen_alloc_wt_tech_prod ~ (plan_alloc_wt_tech_prod - scen_alloc_wt_tech_prod) / abs(scen_alloc_wt_tech_prod - reference_value),
        TRUE ~ -(plan_alloc_wt_tech_prod - scen_alloc_wt_tech_prod) / (scen_alloc_wt_tech_prod - reference_value))
    ) %>% 
    mutate(build_out_deviation_sec = (plan_sec_emissions_factor - scen_sec_emissions_factor) / scen_sec_emissions_factor) %>% 
    mutate(
      build_out_deviation_sec = if_else(scen_sec_emissions_factor == 0, if_else(plan_sec_emissions_factor == 0, 0, -1), build_out_deviation_sec),
      build_out_deviation = if_else(ald_sector %in% other_sectors,build_out_deviation_sec, build_out_deviation_tech)
    ) %>% 
    mutate(
      build_out_alignment = case_when(
        ald_sector %in% other_sectors ~ -build_out_deviation,
        technology %in% high_carbon_technologies ~ -build_out_deviation,
        TRUE ~ build_out_deviation
      )
    ) %>% 
    ungroup()
}

summarise_portfolio_paris_alignment <- function(
  .data, 
  portfolio,
  alignment_from = "trajectory_alignment",
  start_year = 2019
) {
  # apply weight to get portfolio results 
  .data %>% 
    filter(between(.data$year, start_year, start_year + 5)) %>% 
    influencemap_weighting_methodology(alignment_from = alignment_from)
}

add_nice_values <- function(
  data, 
  values_from = "paris_alignment_score", 
  scale = 100,
  min = -200, 
  max = 200
) {
  data %>% 
    assertr::verify(class(.data[[values_from]]) == "numeric") %>% 
    mutate(
      "{values_from}" := case_when(
        .data[[values_from]] * scale > max ~ max,
        .data[[values_from]] * scale < min ~ min,
        TRUE ~ .data[[values_from]] * scale
      )
    )
}

classify_asset_types <- function(.data) {
  # sovereign debt types 
  sovereign_types <- c(
    "Supra-National Debt", 
    "Sovereign Agency Debt", 
    "Treasury Bills", 
    "Sovereign Debt", 
    "Local/Regional Govt Debt", 
    "U.S. Taxable Municipals",
    "Government inflation linked Bonds",
    "U.S. Treasuries",
    "Sovereign",
    "Sovereign Agency",
    "Sovereigns"
  )
  # classify asset-types 
  .data %>% 
    assertr::verify(has_all_names("security_bics_subgroup", "security_type")) %>% 
    mutate(
      asset_type = if_else(tolower(security_bics_subgroup) %in% tolower(sovereign_types), "sovereign", asset_type),
      asset_type = if_else(tolower(security_type) %in% tolower(sovereign_types), "sovereign", asset_type),
      asset_type = if_else(asset_type %in% c("others", "unclassifiable"), "other", asset_type)
    )
}

prep_debt_economy <- function() {
  browntechs <- c("Oil","Gas","Coal","CoalCap","GasCap","ICE")
  
  debt_economy <- read_rds(path(analysis_inputs_path, "bonds_ald_scenario_long", ext = "rda")) %>% 
    filter(
      (ald_sector == "Power" & scenario_geography == "GlobalAggregate") | (ald_sector != "Power" & scenario_geography == "Global"),
      year %in% c(start_year, start_year + 5),
      !is.na(scenario)
    ) %>%  
    select(
      scenario, 
      ald_sector, 
      technology, 
      year, 
      plan_tech_prod, 
      plan_emission_factor, 
      plan_build_out, 
      plan_sector_prod,
      scen_tech_prod, 
      scen_emission_factor, 
      scen_build_out, 
      scen_sec_prod
    )
  
  debt_economy <- debt_economy %>% 
    mutate_at(
      .vars = c("ald_sector", "technology"),
      .funs = tolower
    )
  
  debt_economy <- debt_economy %>% 
    group_by(
      scenario, ald_sector, technology, year
    ) %>% 
    summarise(
      plan_emission_factor = weighted.mean(plan_emission_factor, plan_tech_prod, na.rm = T),
      scen_emission_factor = weighted.mean(scen_emission_factor, scen_tech_prod, na.rm = T),
      
      plan_tech_prod = sum(plan_tech_prod, na.rm = T),
      scen_tech_prod = sum(scen_tech_prod, na.rm = T),
    ) %>% ungroup() %>% 
    mutate(
      trajectory_deviation = ifelse(scen_tech_prod == 0, ifelse(plan_tech_prod == 0, 0, -1), (plan_tech_prod - scen_tech_prod) / scen_tech_prod),
      trajectory_alignment =  ifelse(technology %in% browntechs, 1 * trajectory_deviation, -1 * trajectory_deviation),
      
      plan_emission_factor = if_else(is.nan(plan_emission_factor),0,plan_emission_factor),
      scen_emission_factor = if_else(is.nan(scen_emission_factor),0,scen_emission_factor)
    ) %>% 
    group_by(
      scenario, ald_sector, technology
    ) %>% 
    mutate(
      plan_build_out = plan_tech_prod - sum(if_else(year == start_year,plan_tech_prod,0),na.rm = T),    
      scen_build_out = scen_tech_prod - sum(if_else(year == start_year,scen_tech_prod,0),na.rm = T)
    ) %>% 
    group_by(
      scenario, ald_sector, year
    ) %>% 
    mutate(
      plan_sec_emissions_factor = weighted.mean(plan_emission_factor, plan_tech_prod, na.rm = T),
      scen_sec_emissions_factor = weighted.mean(scen_emission_factor, scen_tech_prod, na.rm = T),
      
      plan_sector_prod = sum(plan_tech_prod, na.rm = T),
      scen_sector_prod = sum(scen_tech_prod, na.rm = T),
      
      plan_tech_share = plan_tech_prod / plan_sector_prod,
      scen_tech_share = scen_tech_prod / scen_sector_prod
    ) %>% 
    ungroup()
  
  economy_sector_co2_intensity_data <- debt_economy %>% 
    filter(year == start_year) %>% 
    transmute(
      ald_sector, 
      traffic_light_yellow = plan_sec_emissions_factor
    ) %>%
    distinct()
  
  # ~ traffic light system ~
  traffic_light_sector_co2_intensity_data <- debt_economy %>% 
    filter(
      year == start_year+5, 
      scenario %in% c("B2DS","SBTI")
    ) %>% 
    transmute(
      ald_sector, 
      traffic_light_green = scen_sec_emissions_factor
    ) %>%
    distinct() %>% 
    full_join(economy_sector_co2_intensity_data, by = "ald_sector")
  
  # unit transformation for transport sectors
  traffic_light_sector_co2_intensity_data <- traffic_light_sector_co2_intensity_data %>% 
    mutate(
      traffic_light_yellow = if_else(ald_sector %in% c("shipping", "aviation", "automotive"), traffic_light_yellow * 1e6, traffic_light_yellow),
      traffic_light_green = if_else(ald_sector %in% c("shipping", "aviation", "automotive"), traffic_light_green * 1e6, traffic_light_green)
    ) %>% 
    transmute(
      indicator_name = paste0("sector_co2_intensity_", ald_sector),
      traffic_light_yellow,
      traffic_light_green
    )
  
  traffic_light_technology_share <- debt_economy %>% 
    filter(
      scenario %in% c("B2DS","SBTI"), 
      year == start_year+5, 
      ald_sector %in% c("power","automotive"),
      !is.na(plan_tech_share)
    ) %>% 
    transmute(
      indicator_name = paste0("technology_share_",str_remove(technology,"cap")),
      traffic_light_yellow = plan_tech_share * 100, 
      traffic_light_green = scen_tech_share * 100
    ) %>% 
    distinct()
  
  traffic_light_technology_climate_alignment <- debt_economy %>% 
    filter(
      year == start_year+5,
      technology %in% c("renewablescap", "electric"),
      scenario == "B2DS") %>% 
    mutate(
      technology_climate_alignment = plan_build_out / scen_build_out * 100
    ) %>% 
    transmute(
      indicator_name = paste0("climate_alignment_", str_remove(technology,"cap")),
      traffic_light_yellow = round(if_else(technology_climate_alignment>200,200.00,technology_climate_alignment),2),
      traffic_light_green = 100
    ) %>% 
    distinct() 
  
  traffic_light_values <- bind_rows(
    traffic_light_sector_co2_intensity_data,
    traffic_light_technology_climate_alignment,
    traffic_light_technology_share
  ) %>% 
    mutate(
      traffic_light_yellow = signif(traffic_light_yellow,3),
      traffic_light_green = signif(traffic_light_green,3),
      traffic_light_value_grey = NA
    )
}

add_several_nice_names <- function(
  .data,
  values_from = NULL, 
  names_from = NULL, 
  values_to = "value", 
  names_to = "indicator", 
  names_prefixes = NULL,
  names_suffixes = NULL,
  sep = "_",
  ...
) {
  purrr::map(
    1:length(values_from), 
    function(x) {
      value_x <- values_from[x]
      selected_columns <- c(id_cols, value_x, names_from)
      .data %>% 
        distinct(!!!rlang::syms(selected_columns)) %>% 
        add_nice_names(
          values_from = value_x,
          values_to = values_to,
          names_from = names_from, 
          names_to = names_to,
          names_prefix = names_prefixes[x],
          names_suffix = names_suffixes[x], 
          sep = sep, 
          ...
        )
    }
  )
}

add_nice_names <- function(
  .data,
  values_from = NULL, 
  names_from = NULL, 
  values_to = "value", 
  names_to = "indicator", 
  names_prefix = NULL,
  names_suffix = NULL,
  sep = "_",
  add_value_class = TRUE,
  ...
) {
  # define parameters 
  character_parameters <- c(names_from, values_from, values_to, names_to, sep)
  character_null_parameters <- c(names_prefix, names_suffix)
  # check parameters
  stopifnot(is.data.frame(.data))
  lapply(character_parameters, function(x) {stopifnot(is.character(x) & !is.null(x))})
  lapply(character_null_parameters, function(x) {stopifnot(is.character(x) | is.null(x))})
  # check names
  crucial_names <- c(names_from, values_from)
  r2dii.utils::check_crucial_names(.data, crucial_names)
  # round values 
  if (is.integer(.data[[values_from]])) {
    .data <- .data %>% 
      dplyr::mutate("{values_from}" := round(.data[[values_from]], ...))
  }
  # add prefix or suffix 
  if (!is.null(names_prefix) & is.null(names_suffix)) {       
    .data <- .data %>% 
      dplyr::mutate("{names_from}" := paste(names_prefix, .data[[names_from]], sep = sep))
  } else if (!is.null(names_prefix) & !is.null(names_suffix)) {       
    .data <- .data %>% 
      dplyr::mutate("{names_from}" := paste(names_prefix, .data[[names_from]], names_suffix, sep = sep))
  } else if (is.null(names_prefix) & !is.null(names_suffix)) {       
    .data <- .data %>% 
      dplyr::mutate("{names_from}" := paste(.data[[names_from]], names_suffix, sep = sep))
  }
  # add class 
  if (add_value_class == TRUE) {
    .data <- .data %>% 
      mutate(value_class = class(.data[[values_from]]))
  }
  # rename values  
  if (!is.null(values_to)) {
    .data <- .data %>% 
      dplyr::rename("{values_to}" := .data[[values_from]])
  }
  # rename names 
  if (!is.null(values_to)) {
    .data <- .data %>% 
      dplyr::rename("{names_to}" := .data[[names_from]])
  }
  return(.data)
}

pivot_wider_and_clean_names <- function(
  .data, 
  values_from = "group_share", 
  names_from = "portfolio_name", 
  names_prefix = NULL,
  names_suffix = NULL,
  sep = "_",
  values_fill = NA_integer_,
  ...
) {
  .data <- .data %>% 
    dplyr::ungroup()
  # add nice names 
  .data <- .data %>% 
    add_nice_names(
      values_from = values_from, 
      names_from = names_from, 
      values_to = NULL, 
      names_to = NULL, 
      names_prefix = names_prefix,
      names_suffix = names_suffix,
      sep = "_",
      ...
    )
  # pivot wider   
  .data %>% 
    tidyr::pivot_wider(
      names_from = .data[[names_from]], 
      values_from = .data[[values_from]],
      values_fill = values_fill
    )
}

connect_results_with_portfolio_weights <- function(
  results, 
  portfolio, 
  sector_from = "security_mapped_sector",
  sector_to = "ald_sector"
) {
  # check no sectors are dropped 
  dropped_sectors <- setdiff(unique(portfolio[[sector_from]]), unique(results[[sector_to]]))
  warning(glue("PACTA results missing portfolio sectors:\n {usethis::ui_field(dropped_sectors)}"))
  # prepare audit file to show the relative portfolio weight in each sector 
  asset_type_sector_exposure <- portfolio %>% 
    summarise_group_share(
      id_cols = c("investor_name", "portfolio_name"),
      values_from = "value_usd",
      numerator_group = sector_from, 
      denominator_group = "asset_type", 
      values_to = "asset_type_sector_exposure", 
      na.rm = TRUE
    ) %>% 
    rename("{sector_to}" := .data[[sector_from]])
  # join audit exposure to result file 
  results <- results %>%
    inner_join(
      asset_type_sector_exposure, 
      by = c(sector_to, "investor_name", "portfolio_name", "asset_type")
    )
  # prepare audit file to show the relative portfolio weight in each sector 
  sector_exposure <- portfolio %>% 
    summarise_group_share(
      id_cols = "investor_name",
      values_from = "value_usd",
      numerator_group = sector_from, 
      denominator_group = "portfolio_name", 
      values_to = "sector_exposure", 
      na.rm = TRUE
    ) %>% 
    rename("{sector_to}" := .data[[sector_from]])
  # join audit exposure to result file 
  results <- results %>%
    inner_join(
      sector_exposure, 
      by = c(sector_to, "investor_name", "portfolio_name")
    )
  # asset_type exposure
  asset_type_exposure <- portfolio %>% 
    summarise_group_share(
      id_cols = "investor_name",
      values_from = "value_usd",
      numerator_group = "asset_type", 
      denominator_group = "portfolio_name", 
      values_to = "asset_type_exposure", 
      na.rm = TRUE
    )
  # join asset type exposure with results 
  results %>%
    inner_join(
      asset_type_exposure, 
      by = c("investor_name", "portfolio_name", "asset_type")
    )
}

filter_unique_cross_sector_results <- function(
  .data, 
  scenario = "b2ds",
  allocation = "portfolio_weight",
  start_year = 2019
) {
  .data %>% 
    ungroup() %>% 
    filter(
      .data$scenario == "b2ds", 
      .data$scenario_geography == "global",
      .data$allocation == "portfolio_weight",
      between(.data$year, start_year, start_year + 5), 
    )
}

load_portfolio <- function(project_location) {
  # load portfolio
  portfolio <- read_rds(path(project_location, "30_Processed_Inputs", paste(project_name, "total_portfolio", sep = "_"), ext = "rda"))
  # clean names 
  portfolio <- portfolio %>% 
    mutate(across(matches(c("asset_type", "financial_sector", "security_mapped_sector")), tolower))
  # classify asset_types 
  portfolio %>% 
    classify_asset_types()
  
  
}

#prepare PACTA results
load_results <- function(project_location = project_location) {
  # load results 
  portfolio_debt_results <- read_rds(path(project_location, "40_Results", "Bonds_results_portfolio", ext = "rda")) %>% 
    clean_names(case = "snake")
  portfolio_equity_results <- read_rds(path(project_location, "40_Results", "Equity_results_portfolio", ext = "rda")) %>% 
    clean_names(case = "snake") 
  # add asset-type 
  portfolio_debt_results$asset_type <- "bonds"
  portfolio_equity_results$asset_type <- "equity"
  # bind things together
  portfolio_results <- portfolio_equity_results %>% 
    bind_rows(portfolio_debt_results) 
  # to lower selection
  columns_to_lower <- c(
    "ald_sector", 
    "technology", 
    "asset_type", 
    "scenario", 
    "equity_market", 
    "scenario_geography", 
    "ald_sector", 
    "technology"
  )
  # make names nicers 
  portfolio_results %>% 
    mutate(across(matches(columns_to_lower), tolower)) %>% 
    ungroup()
}

# load data sets if needed (check indicator list for this)
load_fund_results <- function(
  git_path,
  project_location
){
  # Other Data: sector, revenue, and scenario data
  # TODO: check if all is needed
  
  # prep influencemap sector and technology weights 
  sector_weightings <<- read_csv(path(git_path, "data", "sector_weightings", ext = "csv"))  %>% 
    clean_names(case = "snake") %>%
    mutate(across(matches(c("sector", "technology")), tolower)) 
  
  #prep scenario relationships file 
  # scenario_relationships <- read_csv(paste0(git_path, "Reference/ReferenceData/scenario_relationships.csv"))
  # 
  # scenario_relationships <<- scenario_relationships %>% 
  #   clean_names(case = "snake") %>% 
  #   mutate_at(
  #     .vars = c("sector"),
  #     tolower
  #   )
  
  # prepare revenue data 
  # revenue_data <- read_rds(path(git_path, "Reference/ReferenceData/revenue_data", ext = "rda"))
  # 
  # revenue_data <- revenue_data %>% 
  #   clean_names(case = "snake") %>% 
  #   mutate_at(
  #     .vars = c("sector", "asset_type", "sub_sector"),
  #     tolower
  #   )
  # 
  # revenue_data <<- revenue_data %>% 
  #   mutate(tot_rev = if_else(tot_rev > 100, 100, tot_rev))
  
  # prepare sensitive sectors 
  # sensitive_sectors <- read_csv(paste0(git_path, "Reference/ReferenceData/sensentive_sectors.csv"))
  # 
  # sensitive_sectors <- sensitive_sectors %>% 
  #   clean_names(case = "snake") %>% 
  #   mutate_at(
  #     .vars = c("asset_type", "sector"),
  #     .funs = tolower
  #   )
  # 
  # sensitive_sectors <- sensitive_sectors %>% 
  #   mutate(
  #     tot_rev = if_else(tot_rev > 100, 100, tot_rev),
  #     id = as.character(id)
  #     )
  # 
  # sensitive_sectors <- sensitive_sectors %>% 
  #   bind_rows(revenue_data) %>% 
  #   distinct_all()
  
  # prepare regions 
  regions <<- read_csv(path(git_path, "data", "regions", ext = "csv"))
  
  # indicator selection
  project_indicator_selection <<- read_csv(path(project_location, "10_Parameter_File", "fund_results_indicator_selection", ext = "csv"))
  
  # raw MorningStar data
  general_fund_data <<- read_csv(path(project_location, "20_Raw_Inputs", paste(project_name, "fund_info", sep = "_"), ext = "csv")) %>% 
    clean_names(case = "snake")
  
  portfolio_results <<- load_pacta_results(project_location) 
  
  audit_file <<- load_pacta_portfolio(project_location) 
  
  # Ports.Overview <<- read_csv(paste0(project_location, paste(project_name, "overview_portfolio", sep = "_"), ext = "csv"))
  
  # TODO: determine company with highest coal % in power sector (use filter of at least 100 MW?)
  # pacta_company_debt_results <- read_rds(paste0(project_location, "40_Results/Bonds_results_company.rds"))
  # pacta_company_equity_results <- read_rds(paste0(project_location, "40_Results/Equity_results_company.rds"))
  # 
  # pacta_company_debt_results$asset_type <- "Bonds"
  # pacta_company_equity_results$asset_type <- "Equity"
  # 
  # pacta_company_results <- pacta_company_debt_results %>% 
  #   bind_rows(pacta_company_equity_results)
  # 
  # pacta_company_results <- pacta_company_results %>% 
  #   filter((ald_sector == "Power" & scenario_geography == "GlobalAggregate") | (ald_sector != "Power" & scenario_geography == "Global")
  #   )
  # 
  # pacta_company_results <- pacta_company_results %>% 
  #   filter(investor_name != portfolio_name | portfolio_name == "Meta Portfolio")
  # 
  # pacta_company_results <- pacta_company_results %>% 
  #   clean_names(case = "snake") %>% 
  #   mutate_at(
  #     .vars = c("ald_sector", "technology", "asset_type"),
  #     .funs = tolower
  #   )
  # 
  # input_pacta_company_results <<- pacta_company_results
}


check_group_share_parameters <- function(
  .data, 
  id_cols,
  numerator_group, 
  denominator_group,
  values_from,
  values_to
) {
  character_parameters <- c(numerator_group, denominator_group, values_from)
  character_null_parameters <- c(id_cols, values_to)
  # check parameters 
  lapply(character_parameters, function(x) {stopifnot(is.character(x) & !is.null(x))})
  lapply(character_null_parameters, function(x) {stopifnot(is.character(x) | is.null(x))})
  # check names 
  crucial_names <- c(values_from, numerator_group, denominator_group, id_cols)
  r2dii.utils::check_crucial_names(.data, crucial_names)
  # check values
  assertr::verify(.data, class(.data[[values_from]]) == "numeric")
  # return 
  return(.data)
}

summarise_group_share <- function(
  .data,
  id_cols = NULL,
  numerator_group = "asset_type", 
  denominator_group = "portfolio_name",
  values_from = "value_usd",
  values_to = NULL,
  ...
) {
  # ungroup data 
  .data <- .data %>% 
    ungroup()
  # check parameters
  .data <- .data %>% 
    check_group_share_parameters(
      id_cols = id_cols,
      numerator_group = numerator_group, 
      denominator_group = denominator_group,
      values_from = values_from,
      values_to = values_to
    )
  # define group_vars 
  group_vars <- unique(c(id_cols, denominator_group))
  # calculate exposure 
  .data <- .data %>%
    dplyr::group_by(!!!rlang::syms(c(group_vars, numerator_group))) %>% 
    dplyr::summarise(group_share = sum(.data[[values_from]], ...)) %>% 
    group_by(!!!rlang::syms(group_vars)) %>% 
    dplyr::mutate(group_share = .data$group_share / sum(.data$group_share, ...)) %>% 
    dplyr::ungroup()
  # rename output
  if (!is.null(values_to)) {
    .data %>% 
      dplyr::rename("{values_to}" := .data$group_share)
  } else if (is.null(values_to)) {
    return(.data)
  }
}