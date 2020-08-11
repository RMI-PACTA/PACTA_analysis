library(tidyverse)
library(janitor)
library(assertr)
library(fs)

clean_and_pivot <- function(
  .data, 
  values_from = "pacta_sector_exposure", 
  names_from = "financial_sector", 
  names_prefix = NULL,
  names_suffix = NULL,
  sep = "_",
  ...
) {
  # round values 
  .data <- .data %>% 
    mutate(values = round(.data[[values_from]], ...))
  # add prefix or suffix 
  if (!is.null(names_prefix) & is.null(names_suffix)) {       
    .data <- .data %>% 
      mutate(names = paste(names_prefix, .data[[names_from]], sep = sep))
  } else if (!is.null(names_prefix) & !is.null(names_suffix)) {       
    .data <- .data %>% 
      mutate(names =  paste(names_prefix, .data[[names_from]], names_suffix, sep = sep))
  } else if (is.null(names_prefix) & !is.null(names_suffix)) {       
    .data <- .data %>% 
      mutate(names =  paste(.data[[names_from]], names_suffix, sep = sep))
  }
  # pivot wider   
  .data %>% 
    pivot_wider(
      names_from = .data$names, 
      values_from = .data$values,
      values_fill = list(values = 0)
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
    r2dii.utils::check_crucial_names(crucial_names)
  # check market value class
  data <- data %>% 
    assertr::verify(class(.data[[market_value_from]]) == "numeric")
  # check for negative values 
  data <- data %>% 
    assertr::assert(
      within_bounds(0,Inf),
      .data[[market_value_from]], 
      error_fun = just_warn
    )
  # check isin inputs 
  data %>% 
    filter(str_detect(.data$isin, "[[:upper:]]{2}[[:alnum:]]{10}")) %>% 
    assertr::verify(nrow(.) > 0)
}

calculate_sensentive_exposures <- function(
  .data, 
  id_cols = c("investor_name", "portfolio_name"),
  market_value_from = "value_usd",
  ...
) {
  # define crucial names 
  crucial_names <- c(id_cols, market_value_from, "isin")
  # check portfolio data 
  .data <- .data %>% 
    check_portfolio_data(crucial_names, market_value_from)
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
    mutate(adjusted_market_value = .data$market_value / n())
}

load_esg_raw_data <- function() {
  # list files 
  files <- list.files(path("data", "iss_esg_data", "company_exposures"))
  # check file count 
  stopifnot(n_distinct(files) == 11)
  # load and map files 
  data <- map_df(
    list.files(path("data", "iss_esg_data")), 
    function(x) {
      readxl::read_xlsx(path("data", "iss_esg_data", x)) %>% 
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
  name_to = NULL
) {
  # check parameters 
  stopifnot(is.character(id_cols))
  stopifnot(is.character(weights_from))
  stopifnot(is.character(values_from))
  stopifnot(is.character(name_to) | is.null(name_to))
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
  name_to = NULL, 
  ...
) {
  # check parameters 
  .data <- .data %>% 
    check_group_weight_parameters(
      id_cols,
      weights_from,
      values_from,
      name_to
    )
  # weight value 
  .data <- .data %>% 
    dplyr::group_by(!!!rlang::syms(id_cols)) %>% 
    dplyr::summarise(weighted_value = stats::weighted.mean(.data[[values_from]], .data[[weights_from]])) %>% 
    dplyr::ungroup()
  # rename output
  if (!is.null(name_to)) {
    .data %>% 
      plyr::rename(c("weighted_value" = name_to))
  } else if (is.null(name_to)) {
    return(.data)
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

check_group_share_data <- function(
  data, 
  numerator_group, 
  denominator_group,
  values_from
) {
  # ungroup input 
  data <- data %>%
    dplyr::ungroup()
  # check essential names
  data %>%
    r2dii.utils::check_crucial_names(c(values_from, numerator_group, denominator_group)) %>% 
    assertr::verify(class(.data[[values_from]]) == "numeric")
}

check_group_share_parameters <- function(
  id_cols,
  numerator_group, 
  denominator_group,
  values_from,
  name_to
) {
  stopifnot(is.character(id_cols) | is.null(id_cols))
  stopifnot(is.character(numerator_group))
  stopifnot(is.character(denominator_group))
  stopifnot(is.character(values_from))
  stopifnot(is.character(name_to) | is.null(name_to))
}

summarise_group_share <- function(
  .data,
  id_cols = NULL,
  numerator_group = "technology", 
  denominator_group = "ald_sector",
  values_from = "plan_alloc_wt_tech_prod",
  name_to = NULL,
  ...
) {
  # check parameters
  check_group_share_parameters(
    id_cols,
    numerator_group, 
    denominator_group,
    values_from,
    name_to
  )
  # check input logic 
  .data <- .data %>% 
    check_group_share_data(
      numerator_group, 
      denominator_group,
      values_from
    )
  # calculate exposure 
  .data <- .data %>%
    dplyr::group_by(
      !!!rlang::syms(id_cols),
      .data[[numerator_group]],
      .data[[denominator_group]]
    ) %>% 
    dplyr::summarise(group_share = sum(.data[[values_from]], ...)) %>% 
    group_by(
      !!!rlang::syms(id_cols),
      .data[[denominator_group]]
    ) %>% 
    dplyr::mutate(group_share = .data$group_share / sum(.data$group_share, ...)) %>% 
    dplyr::ungroup()
  # rename output
  if (!is.null(name_to)) {
    .data %>% 
      plyr::rename(c("group_share" = name_to))
  } else if (is.null(name_to)) {
    return(.data)
  }
}

calculate_group_share <- function(
  .data,
  id_cols = NULL,
  numerator_group = "technology", 
  denominator_group = "ald_sector",
  values_from = "plan_alloc_wt_tech_prod",
  name_to = NULL,
  ...
) {
  # check parameters
  check_group_share_parameters(
    id_cols,
    numerator_group, 
    denominator_group,
    values_from,
    name_to
  )
  # check input logic 
  .data <- .data %>% 
    check_group_share_data(
      numerator_group, 
      denominator_group,
      values_from
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
  if (!is.null(name_to)) {
    .data %>% 
      plyr::rename(c("group_share" = name_to))
  } else if (is.null(name_to)) {
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
      .data$allocation == "portfolio_weight",
      between(.data$year, start_year, start_year + 5), 
      (.data$scenario_geography == "globalaggregate" & .data$ald_sector == "power") | (.data$ald_sector != "power" & .data$scenario_geography == "global")
    )
}

summarise_portfolio_paris_alignment <- function(
  results, 
  portfolio,
  alignment_from = "trajectory_alignment",
  start_year = 2019,
  git_path = here::here()
) {
  # apply weight to get portfolio results 
  influencemap_weighting_methodology(results, portfolio)
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

# gather multiple result files into one for, necessary for large universes
#### Collate Results for Peer Comparison ####
gather_all_company_result_files <- function(result_path = RESULTS.PATH){
  all_files_list_bonds <- list.files(result_path,pattern = "Bonds_results_company")
  all_files_list_equity <- list.files(result_path,pattern = "Equity_results_company")
  
  for (i in 1:length(all_files_list_bonds)){
    
    print(paste0(i," of ", length(all_files_list_bonds)))
    
    #PACTA results
    input_debt <- readRDS(paste0(result_path,all_files_list_bonds[i]))
    input_equity <- readRDS(paste0(result_path,all_files_list_equity[i]))
    
    if(!is.na(input_equity)){
      if (exists("AllEQPortResults")){
        AllEQPortResults <- rbind(AllEQPortResults, input_equity)
      }else{
        AllEQPortResults <- input_equity
      }}
    
    if(!is.na(input_debt)){
      if (exists("AllBondPortResults")){
        AllBondPortResults <- rbind(AllBondPortResults, input_debt)
      }else{
        AllBondPortResults <- input_debt
      }}
  }
  
  saveRDS(AllBondPortResults,paste0(result_path,"Bonds_results_company.rda"))
  saveRDS(AllEQPortResults,paste0(result_path,"Equity_results_company.rda"))}

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

group_ranker <- function(input_audit, n_top = 5, group_var, group_name = "sectors", remove = c("Sovereign", "Sovereign Agency", "Sovereigns")) {
  
  if (group_name == "countries") {
    
    holdings_view <- input_audit %>% select(portfolio_name, country_of_domicile, value_usd, isin, asset_type, flag, holding_id) %>% distinct() %>%
      filter(!is.na(country_of_domicile) & !is.na(value_usd) & !country_of_domicile %in% c(remove)) %>% 
      # left_join(regions, 
      # by = c("country_of_domicile" = "alpha-2")) 
      mutate(name = country_of_domicile)
    
  } else {
    
    # clean input 
    holdings_view <- input_audit %>% select({{ group_var }}, portfolio_name, country_of_domicile, value_usd, isin, asset_type, flag, holding_id) %>% distinct() %>%
      rename(name := {{ group_var }}) %>% 
      filter(!is.na(name) & !is.na(value_usd) & !name %in% c(remove)) 
    
  }
  
  # summarize group variable 
  portfolio_view <- holdings_view %>% 
    group_by(portfolio_name, name) %>%
    summarise(
      value_usd_group = sum(as.numeric(value_usd))
    )
  
  #calculate percentage exposure     
  portfolio_view <- portfolio_view %>% 
    group_by(portfolio_name) %>% 
    mutate(
      share = value_usd_group/sum(value_usd_group, na.rm = T)
    )
  
  portfolio_view <- portfolio_view %>%
    group_by(portfolio_name) %>%
    arrange(desc(share)) %>%
    top_n(n = n_top, share)
  
  portfolio_view <- portfolio_view %>% 
    group_by(portfolio_name) %>%
    mutate(
      n_rank = seq_along(share)
    ) %>% 
    filter(n_rank <= n_top) %>% 
    select(-value_usd_group) %>% ungroup()
  
  portfolio_view <- portfolio_view %>% 
    mutate(
      n_rank = paste0("top_", group_name, "_", n_rank)
    )
  
  portfolio_view <- portfolio_view %>% 
    pivot_wider(names_from = n_rank,
                values_from = c(share, name),
                names_sep = "_")
  
  return(portfolio_view)
  
}

summarise_group_exposure <- function(input, group_vars, rel_vars, value_var, name_var = NULL){
  
  input <- input %>%
    group_by(!!! syms(group_vars)) %>%
    summarise(
      value_var_group = sum(.data[[value_var]], na.rm = T)
    )
  
  output <- input %>% 
    group_by(!!! syms(group_vars[!group_vars %in% rel_vars])) %>%
    mutate(
      value_var_exposure = sum(value_var_group, na.rm = T)
    ) %>% 
    select(-value_var_group)
  
  if(!is.null(name_var)){
    
    output <- output %>% 
      rename(.data[[name_var]] == value_var_exposure)
    
  }
  
  return(output)
  
}

# output <- summarise_group_exposure(
#   input = input_audit, # a dataframe or tibble 
#   group_vars = c("Investor.Name", "portfolio_name", "mapped_sector", "asset_type"), # a set of grouping variables. Must include the rel_var!
#   rel_vars = "mapped_sector", # one or more grouping variables to calculate the relative exposure for. 
#   value_var = "ValueUSD" # a numeric value to calculate percentage exposure. 
# )

results <- load_results(project_location)

connect_results_with_weights <- function(
  results, 
  portfolio,
  git_path = here::here()
) {
  # load sector weights 
  sector_weights <- read_rds(path(git_path, "data", "sector_weightings", ext = "rds"))
  # first join technology and sector weights to pacta results file 
  results <- results %>%
    inner_join(
      sector_weights, 
      by = c("ald_sector", "technology")
    )
  # prepare audit file to show the relative portfolio weight in each sector 
  asset_type_sector_exposure <- portfolio %>% 
    summarise_group_share(
      id_cols = c("investor_name", "portfolio_name"),
      values_from = "value_usd",
      numerator_group = "security_mapped_sector", 
      denominator_group = "asset_type", 
      name_to = "sector_exposure", 
      na.rm = TRUE
    ) %>% 
    rename(ald_sector = .data$security_mapped_sector)
  # join audit exposure to result file 
  results <- results %>%
    inner_join(
      asset_type_sector_exposure, 
      by = c("ald_sector", "investor_name", "portfolio_name", "asset_type")
    )
  # asset_type exposure
  asset_type_exposure <- portfolio %>% 
    summarise_group_share(
      id_cols = "investor_name",
      values_from = "value_usd",
      numerator_group = "asset_type", 
      denominator_group = "portfolio_name", 
      name_to = "asset_type_exposure", 
      na.rm = TRUE
    )
  # join asset type exposure with results 
  results %>%
    inner_join(
      asset_type_exposure, 
      by = c("investor_name", "portfolio_name", "asset_type")
    )
}

# Single Indicator function - should be loaded from r2dii.analysis instead!
influencemap_weighting_methodology <- function(
  results,
  portfolio,
  alignment_from = "trajectory_alignment",
  git_path = here::here()
) {
  # crucial names
  crucial_names <- c("investor_name", "portfolio_name", "asset_type", "ald_sector", "scenario", "scenario_geography", "allocation")
  # check inputs 
  r2dii.utils::check_crucial_names(results, crucial_names) 
  # connect results with weights 
  results <- connect_results_with_weights(results, portfolio, git_path)
  # first reweight alignment to the sector level based on the technology importance 
  results <- results %>%
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
        stats::weighted.mean(.data[[alignment_from]], .data$technology_weight * .data$plan_alloc_wt_tech_prod, na.rm = TRUE),
        .data[[alignment_from]]
      )
    )
  # then weight up to asset_type level 
  results <- results %>%
    group_by(
      .data$investor_name, 
      .data$portfolio_name, 
      .data$asset_type,
      .data$asset_type_exposure,
      .data$scenario, 
      .data$scenario_geography, 
      .data$allocation
    ) %>%
    summarise(alignment_asset_type = stats::weighted.mean(.data$alignment_sector, .data$sector_weight * .data$sector_exposure, na.rm = TRUE))
  # calculate portfolio level results 
  results %>%
    group_by(
      .data$investor_name, 
      .data$portfolio_name,
      .data$scenario, 
      .data$scenario_geography, 
      .data$allocation
    ) %>%
    summarise(paris_alignment_score = stats::weighted.mean(.data$alignment_asset_type, .data$asset_type_exposure, na.rm = TRUE)) %>% 
    ungroup()
}

mapped_sector_exposure <- function(
  input_audit = pacta_audit, 
  group_vars,
  fund_size_data, 
  fund_size_indicator,
  sector_list = c("power", "shipping", "aviation", "oil&gas", "steel", "coal", "cement", "automotive")
) {
  
  #################################################################
  # coverage assessment for the single indicator metric
  #################################################################
  
  coverage <- input_audit %>%
    mutate(climate_rel_sector = ifelse(tolower(security_mapped_sector) %in% tolower(sector_list), TRUE, FALSE)) %>% 
    distinct(
      !!!syms(group_vars), 
      value_usd,
      climate_rel_sector
    )
  
  fund_size_data <- fund_size_data %>% 
    distinct(
      !!!syms(group_vars), 
      .data[[fund_size_indicator]]
    )
  
  coverage <- coverage %>%
    left_join(
      fund_size_data,
      by = group_vars
    )
  
  coverage <- coverage %>%
    group_by(
      !!!syms(group_vars), 
      climate_rel_sector
    ) %>%
    mutate(
      sector_exposure_climate_alignment_sectors = sum(value_usd, na.rm = TRUE) / .data[[fund_size_indicator]],
      sector_exposure_climate_alignment_sectors = ifelse(is.na(sector_exposure_climate_alignment_sectors), 0 , sector_exposure_climate_alignment_sectors)
    ) %>%
    ungroup()
  
  
  coverage %>%
    filter(climate_rel_sector == TRUE) %>%
    distinct(
      !!!syms(group_vars), 
      sector_exposure_climate_alignment_sectors
    )
}

calculate_technology_sector_exposure <- function(
  input_results,
  input_audit,
  technology_list = c("coalcap", "renewablescap")
) {
  
  sector_list <- c("steel", "cement", "aviation", "shipping")
  
  # filtering and summarizing 
  results_view <- input_results %>% 
    filter(year >= START.YEAR & year <= START.YEAR + 5 & !ald_sector %in% sector_list)
  
  results_view <- results_view %>% 
    filter(!is.na(id)) %>% 
    group_by(
      investor_name,
      portfolio_name, 
      ald_sector, 
      technology, 
      id, 
      asset_type
    ) %>% 
    summarise(plan_alloc_prod = sum(plan_alloc_wt_tech_prod, na.rm = TRUE)) %>% 
    ungroup()
  
  # create audit view to connect to equity view 
  audit_view <- input_audit %>% 
    filter(ValueUSD > 0) %>% 
    select(
      investor_name, portfolio_name, isin, 
      ValueUSD, asset_type, bloomberg_id,
      CorpBondTicker
    )
  
  # create universal id 
  audit_view <- audit_view %>% 
    mutate(
      id = case_when(
        !is.na(bloomberg_id) & asset_type == "Equity" ~ bloomberg_id, 
        !is.na(CorpBondTicker) & asset_type == "Bonds" ~ CorpBondTicker
      )
    ) %>% 
    filter(!is.na(id)) %>% 
    select(-c(bloomberg_id, CorpBondTicker))
  
  # cleaning revenue data 
  revenue_view <- revenue_data %>% 
    filter(!is.na(Sector)) %>% 
    mutate(
      Tot.Rev = Tot.Rev / 100
    )
  
  # add revenue data to audit file 
  audit_view <- audit_view %>% 
    left_join(revenue_view, 
              by = c("isin", 
                     "id" = "ID", 
                     "asset_type" = "asset_type"))
  
  # connect audit to equity 
  results_audit_view <- results_view %>% 
    left_join(audit_view, 
              by = c("id", "asset_type", "ald_sector" = "Sector", "portfolio_name" = "Portfolio.Name", "investor_name" = "Investor.Name",
                     "technology" = "Sub.Sector"))
  
  # calculating exposure when no revue data by production 
  results_audit_view <- results_audit_view %>% 
    group_by(id, ald_sector, technology,portfolio_name, investor_name) %>% 
    mutate(
      prod_technology = ifelse(
        is.na(Tot.Rev),
        sum(plan_alloc_prod, na.rm = T),
        NA
      )
    )
  
  results_audit_view <- results_audit_view %>% 
    group_by(id, ald_sector, portfolio_name, investor_name) %>% 
    mutate(
      Tot.Rev = ifelse(
        is.na(Tot.Rev),
        (prod_technology / sum(plan_alloc_prod, na.rm = T)),
        Tot.Rev
      )
    ) %>% 
    select(-prod_technology)
  
  
  # recalculate value with rev. 
  results_audit_view <- results_audit_view %>% 
    mutate(
      value_usd_rev = Tot.Rev * ValueUSD
    )
  
  # calculate relative exposure for each technology 
  results_audit_view <- results_audit_view %>% 
    group_by(portfolio_name, investor_name,ald_sector) %>% 
    mutate(
      value_usd_rev_sector = sum(value_usd_rev, na.rm = T),
      percent_exposure_holding = value_usd_rev / value_usd_rev_sector
    )
  
  # clean NA's and summarize for each portfolio exposure 
  results_audit_view <- results_audit_view %>% 
    filter(!is.na(ald_sector) & !is.na(technology)) %>% 
    group_by(portfolio_name, ald_sector, technology) %>% 
    summarise(
      percent_exposure_technology = sum(percent_exposure_holding, na.rm = T)
    )
  
  # cleaning output 
  results_audit_view <- results_audit_view %>% 
    mutate(
      percent_exposure_technology = round(percent_exposure_technology, digits = 2)
    )
  
  # taking only technologies we care about 
  results_audit_view <- results_audit_view %>% 
    filter(technology %in% technology_list) %>% 
    mutate(
      technology = str_remove(technology, "Cap")
    )
  
  # unit sector and technology column 
  results_audit_view <- results_audit_view %>% 
    unite(col = "technology", 
          sep = "_", 
          technology, ald_sector) %>% 
    mutate(
      technology = tolower(technology)
    )
  
  
  # pivot wider 
  results_audit_view_wide <- results_audit_view %>% 
    pivot_wider(names_from = technology, 
                values_from = percent_exposure_technology,
                names_prefix = "sector_exposure_")
  
  return(results_audit_view_wide)
  
}


calculate_sensitive_sector_exposure <- function(
  input_audit,
  sector_list,
  sensitive_sectors_data,
  flag,
  fund_size_data, 
  fund_size_indicator
) {
  
  audit_view <- input_audit %>% 
    filter(value_usd > 0) %>% 
    distinct(
      investor_name, 
      portfolio_name, 
      isin, 
      holding_id,
      value_usd, 
      asset_type, 
      bloomberg_id,
      corporate_bond_ticker
    )
  
  # create universal id 
  audit_view <- audit_view %>% 
    mutate(
      id = case_when(
        !is.na(bloomberg_id) & asset_type == "equity" ~ as.character(bloomberg_id), 
        !is.na(corporate_bond_ticker) & asset_type == "bonds" ~ as.character(corporate_bond_ticker)
      )
    )
  
  audit_view <- audit_view %>% 
    filter(!is.na(id)) %>% 
    select(
      -bloomberg_id, 
      -corporate_bond_ticker
    )
  
  # cleaning revenue data 
  sensitive_sectors_data <- sensitive_sectors_data %>% 
    filter(!is.na(sector) & sector %in% sector_list) %>% 
    mutate(tot_rev = tot_rev / 100)
  
  # connect to senstive sector data 
  audit_view <- audit_view %>% 
    left_join(
      sensitive_sectors_data, 
      by = c("isin", "id", "asset_type")
    )
  
  # relative exposure 
  audit_view <- audit_view %>% 
    mutate(value_usd_rev = tot_rev * value_usd)
  
  # calculate relative exposure for each sector 
  audit_view <- audit_view %>% 
    filter(!is.na(sector)) %>% 
    group_by(
      portfolio_name, 
      investor_name, 
      sector
    ) %>% 
    summarise(value_usd_rev_sector = sum(value_usd_rev, na.rm = T)) %>% 
    ungroup()
  
  # merge portfolio value 
  fund_size_data <- fund_size_data %>% 
    distinct(
      portfolio_name,
      fund_size_covered
    )
  
  audit_view <- audit_view %>% 
    left_join(
      fund_size_data, 
      by = c("portfolio_name")
    )
  
  # cleaning output 
  audit_view <- audit_view %>% 
    mutate(
      percent_exposure_sector = value_usd_rev_sector / .data[[fund_size_indicator]],
      sector = str_replace_all(sector, "[[:space:]]", "_")
    ) %>% 
    select(
      -value_usd_rev_sector, 
      -.data[[fund_size_indicator]]
    )
  
  if (flag == TRUE) {
    audit_view <- audit_view %>% 
      mutate(
        percent_exposure_sector = ifelse(percent_exposure_sector > 0, T, F)
      )
    
    audit_view %>% 
      pivot_wider(
        names_from = sector, 
        values_from = percent_exposure_sector,
        names_prefix = "sector_exposure_",
        values_fill = list(percent_exposure_sector = FALSE)
      )
  } else if (flag == FALSE) {
    audit_view %>% 
      pivot_wider(
        names_from = sector, 
        values_from = percent_exposure_sector,
        names_prefix = "sector_exposure_",
        values_fill = list(percent_exposure_sector = 0)
      )
  }
}

# functions to convert integer values into bool values (as defined in the parameter file)
convert_multiple_integer_to_bool <- function(data, indicators_to_convert){
  data <- data %>% 
    mutate_at(.vars = indicators_to_convert, .funs = convert_int_to_bool)
}

convert_int_to_bool <- function(value){
  value <- case_when(
    as.double(value) > 0 ~ 1,
    as.double(value) == 0 ~ 0,
    T ~ as.numeric(NA))
}

convert_int_to_perc<- function(value){
  value <- if_else(is.na(value),as.numeric(NA),value * 100)
}

summarise_geography_exposure <- function(audit_file, # the portcheck project audit file or portfolio file with country_of_domocile and value_usd
                                         geography_scope, # [a character string] "region" or "country" 
                                         filter_in = NULL, # [list of character strings] a list of regions or countries to include 
                                         filter_out = NULL, # [list of character strings] a list countries to exclude 
                                         pivot_wider, # [TRUE, FALSE] whether to pivot the output wider with nice names  
                                         fund_size_data = complete_fund_matrix, # dataset with the size of the portfolios (could also be the same as the audit file)
                                         fund_size_indicator # name that specifies the name of the column in the fund_size_data set with the fund size data 
) { 
  
  #################################################################
  # coverage assessment for the single indicator metric
  #################################################################
  
  # connect portfolio to ISO region and country data 
  audit_file_view <- audit_file %>% select(portfolio_name, country_of_domicile, value_usd, isin, asset_type, flag, holding_id) %>% distinct() %>% 
    filter(!is.na(country_of_domicile) & !is.na(value_usd)) %>% 
    left_join(
      regions, 
      by = c("country_of_domicile" = "alpha-2")
    ) %>% 
    rename(country = name)
  
  # calculate exposure for a given geography
  audit_file_view <- audit_file_view %>% 
    group_by(
      portfolio_name, 
      .data[[geography_scope]]
    ) %>%
    summarise(value_usd_geography = sum(as.numeric(value_usd))) 
  
  audit_file_view <- audit_file_view %>% 
    left_join(fund_size_data %>% select(fund_isin, .data[[fund_size_indicator]]), by = c("portfolio_name" = "fund_isin"))
  
  
  # find relative exposure 
  audit_file_view <- audit_file_view %>% 
    group_by(
      portfolio_name
    ) %>%
    mutate(geography_exposure = value_usd_geography / .data[[fund_size_indicator]])
  
  
  # filtering for select countries or regions 
  audit_file_view <- audit_file_view %>% 
    filter(.data[[geography_scope]] %in% filter_in) %>% 
    select(
      portfolio_name, 
      geography_exposure, 
      .data[[geography_scope]]
    )
  
  if (geography_scope == "region") {
    
    if (!is.null(filter_out)) {
      if (filter_out %in% regions[["name"]]) {
        message(paste0("Calculating rest of ", filter_in), "...")
        
        audit_file_view <- audit_file_view %>% 
          mutate(region = paste0("rest of ", region))
      }
    } else if (is.null(filter_out)) {
      message(paste0("Not calculating rest of ", filter_in), "...")
    } else if (!is.null(filter_out) & !filter_out %in% regions[["name"]]) {
      stop()
    }
    
  }
  
  if (pivot_wider == TRUE) {
    # pivot_wider 
    audit_file_view <- audit_file_view %>% 
      pivot_wider(
        names_from = .data[[geography_scope]], 
        values_from = geography_exposure,
        names_prefix = "percent_exposure_to_",
        values_fill = list(geography_exposure = 0)
      )
  }
  
  audit_file_view
}


summarise_dictatorship_exposure <- function(audit_file, # the portcheck project audit file or portfolio file with country_of_domocile and value_usd
                                            country_list, # [list of character strings] a of countries to include 
                                            fund_size_data = complete_fund_matrix, # dataset with the size of the portfolios (could also be the same as the audit file)
                                            fund_size_indicator # name that specifies the name of the column in the fund_size_data set with the fund size data 
) {
  
  audit_file_view <- summarise_geography_exposure(
    audit_file = audit_file,
    geography_scope = "country",
    filter_in = country_list,
    filter_out = NULL,
    pivot_wider = FALSE,
    fund_size_data = fund_size_data,
    fund_size_indicator =  fund_size_indicator
  )
  
  audit_file_view %>% 
    group_by(portfolio_name) %>% 
    summarise(geographical_exposure_dictatorships = sum(geography_exposure, na.rm = TRUE))
  
}

# #example 
# summarise_dictatorship_exposure(
#   audit_file = audit_file, 
#   country_list = c("China", "Russia", "Germany")
# )


alignment.calcs <- function(df){
  
  browntechs <- c("Oil","Gas","Coal","CoalCap","GasCap","ICE")
  
  df$trajectory_deviation <- (df$plan_alloc_wt_tech_prod - df$scen_alloc_wt_tech_prod) / df$scen_alloc_wt_tech_prod
  df$trajectory_deviation <- ifelse(df$scen_alloc_wt_tech_prod == 0, ifelse(df$plan_alloc_wt_tech_prod == 0, 0, -1), df$trajectory_deviation)
  
  df$trajectory_alignment <-  ifelse(!df$technology %in% browntechs, 1 * df$trajectory_deviation, -1 * df$trajectory_deviation)
  
  df
}

tech.share.calcs <- function(df){
  df <- df %>%
    ungroup() %>%
    mutate(plan_tech_share = plan_alloc_wt_tech_prod/plan_alloc_wt_sec_prod,
           scen_tech_share = scen_alloc_wt_tech_prod/scen_alloc_wt_sec_prod)
  df
}

check_for_duplicates <- function(
  data_frame,
  id
) {
  
  complete_fund_matrix %>% 
    group_by(.data[[id]]) %>% 
    summarise(count = n()) %>% 
    filter(count > 1) %>% 
    arrange(desc(count))
}




