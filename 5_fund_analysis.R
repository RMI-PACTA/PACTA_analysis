source("0_fund_analysis_functions_v2.R")

# TODO sanity check technology alignment figures. 
# TODO fund scores 
# TODO figure top function indicator, value style

project_name <- "daisy_test5"
project_location <- path(r2dii.utils::path_dropbox_2dii(), "PortCheck_v2", "10_Projects", project_name)

id_cols <- c("investor_name", "portfolio_name")
output_columns <- c(id_cols, "indicator", "value", "value_class")

###########################################
###########################################
# load data 
###########################################
###########################################

portfolio <- load_portfolio(project_location)

results <- load_results(project_location) %>% 
  filter_unique_cross_sector_results() %>% 
  connect_results_with_portfolio_weights(portfolio, sector_from = "financial_sector")

###########################################
###########################################
# calculate asset type exposure 
###########################################
###########################################

asset_type_exposure <- portfolio %>% 
  summarise_group_share(
    id_cols = "investor_name", 
    numerator_group = "asset_type", 
    denominator_group = "portfolio_name", 
    values_from = "value_usd", 
    values_to = "asset_type_exposure", 
    na.rm = TRUE
  ) %>% 
  add_nice_names(
    values_from = "asset_type_exposure",
    names_from = "asset_type",
    names_prefix = "exposure"
  )

###########################################
###########################################
# pacta sector exposure 
###########################################
###########################################

pacta_sector_exposure <- portfolio %>% 
  mutate(financial_sector = if_else(financial_sector == "unclassifiable", "other", financial_sector)) %>% 
  summarise_group_share(
    id_cols = "investor_name",
    values_from = "value_usd",
    values_to = "pacta_sector_exposure", 
    numerator_group = "financial_sector", 
    denominator_group = "portfolio_name", 
    na.rm = TRUE
  ) %>% 
  add_nice_names(
    values_from = "pacta_sector_exposure",
    names_from = "financial_sector",
    names_prefix = "sector_exposure"
  )

###########################################
###########################################
# climate relevant sector exposure 
###########################################
###########################################

climate_relevant_sector_exposure <- portfolio %>% 
  mutate(has_asset_level_data = if_else(is.na(has_asset_level_data), FALSE, has_asset_level_data)) %>% 
  summarise_group_share(
    id_cols = "investor_name",
    values_from = "value_usd",
    values_to = "climate_relevant_sector_exposure", 
    numerator_group = "has_asset_level_data", 
    denominator_group = "portfolio_name", 
    na.rm = TRUE
  )

climate_relevant_sector_exposure <- climate_relevant_sector_exposure %>% 
  filter(has_asset_level_data == TRUE) %>% 
  mutate(indicator = "climate_relevant_sectors_exposure") %>% 
  add_nice_names(
    values_from = "climate_relevant_sector_exposure",
    names_from = "indicator"
  ) %>% 
  distinct(!!!rlang::syms(output_columns))

###########################################
###########################################
# paris alignment score 
###########################################
###########################################

paris_alignment_trajectory_score <- results %>% 
  add_nice_values(
    values_from = "trajectory_alignment", 
    scale = 100,
    min = -100, 
    max = 100
  ) %>% 
  summarise_portfolio_paris_alignment(start_year = 2019, alignment_from = "trajectory_alignment") %>% 
  add_nice_names(
    names_from = "alignment_metric", 
    values_from = "alignment_portfolio"
  )

paris_alignment_build_out_score <- results %>% 
  calculate_build_out_alignment() %>% 
  add_nice_values(
    values_from = "build_out_alignment", 
    scale = 100,
    min = -100, 
    max = 100
  ) %>% 
  summarise_portfolio_paris_alignment(start_year = 2019, alignment_from = "build_out_alignment") %>% 
  add_nice_names(
    names_from = "alignment_metric", 
    values_from = "alignment_portfolio"
  )

alignment_scores <- paris_alignment_trajectory_score %>% 
  bind_rows(paris_alignment_build_out_score) %>% 
  distinct(!!!rlang::syms(output_columns))

###########################################
###########################################
# technology alignment 
###########################################
###########################################

technology_alignment <- results %>% 
  filter(technology == "renewablescap") %>% 
  calculate_technology_alignment(
    start_year = 2019, 
    time_horizon = 5, 
    id_cols = c("investor_name", "portfolio_name", "asset_type", "ald_sector", "technology"),
    plan_tech_prod_from = "plan_alloc_wt_tech_prod", 
    scen_tech_prod_from = "scen_alloc_wt_tech_prod"
  ) %>% 
  add_nice_values(
    values_from = "tech_build_out_alignment", 
    scale = 100,
    min = -200, 
    max = 200
  )

# summarise to portfolio level weighted by asset-type 
technology_alignment <- technology_alignment %>% 
  summarise_group_weight(
    id_cols = c("investor_name", "portfolio_name", "ald_sector", "technology"), 
    weights_from = "asset_type_exposure",
    values_from = "tech_build_out_alignment",
    values_to = "technology_alignment",
    na.rm = TRUE
  )

technology_alignment <- technology_alignment %>% 
  add_nice_names(
    names_from = "technology", 
    values_from = "technology_alignment", 
    names_prefix = "technology_alignment"
  ) %>% 
  distinct(!!!rlang::syms(output_columns))

###########################################
###########################################
# top exposures 
###########################################
###########################################

top_holdings <- portfolio %>% 
  summarise_top_groups(
    n = 5, 
    id_cols = "investor_name",
    values_from = "value_usd",
    values_to = "company_exposure",
    numerator_group = "company_name", 
    denominator_group = "portfolio_name"
  ) %>% 
  add_several_nice_names(
    values_from = c("company_name", "company_exposure"), 
    names_from = "rank", 
    names_prefixes = c("name_top_holdings", "exposure_top_holdings")
  )

top_countries <- portfolio %>% 
  summarise_top_groups(
    n = 5, 
    id_cols = "investor_name",
    values_from = "value_usd",
    values_to = "country_exposure",
    numerator_group = "country_of_domicile", 
    denominator_group = "portfolio_name"
  ) %>% 
  add_several_nice_names(
    values_from = c("country_of_domicile", "country_exposure"), 
    names_from = "rank", 
    names_prefixes = c("name_top_countries", "exposure_top_countries")
  )

top_currencies <- portfolio %>% 
  summarise_top_groups(
    n = 5, 
    id_cols = "investor_name",
    values_from = "value_usd",
    values_to = "currency_exposure",
    numerator_group = "currency", 
    denominator_group = "portfolio_name"
  ) %>% 
  add_several_nice_names(
    values_from = c("currency", "currency_exposure"), 
    names_from = "rank", 
    names_prefixes = c("name_top_currencies", "exposure_top_currencies")
  )

###########################################
###########################################
# sensentive sector exposure
###########################################
###########################################

sensentive_exposures <- portfolio %>% 
  calculate_sensentive_exposures(
    id_cols = c("investor_name", "portfolio_name"), 
    market_value_from = "value_usd"
  )

sensentive_exposures <- sensentive_exposures %>% 
  summarise_group_share(
    id_cols = "investor_name", 
    numerator_group = "sensitive_sector", 
    denominator_group = "portfolio_name", 
    values_from = "adjusted_market_value", 
    values_to = "sensitive_sector_exposure", 
    na.rm = TRUE
  )

sensentive_exposures <- sensentive_exposures %>%
  filter(sensitive_sector != "other") %>% 
  add_nice_names(
    names_from = "sensitive_sector", 
    values_from = "sensitive_sector_exposure", 
    names_prefix = "sector_exposure"
  )

###########################################
###########################################
# weighted emissions factor 
###########################################
###########################################

emission_factors <- results %>% 
  summarise_sector_production_weighted_emission_factor(
    emission_factors_from = "plan_emission_factor", 
    emission_factors_to = "emission_factor",
    production_from = "plan_alloc_wt_tech_prod"
  ) %>% 
  add_nice_names(
    names_from = "ald_sector", 
    values_from = "emission_factor", 
    names_prefix = "sector_co2_intensity"
  )

###########################################
###########################################
# summarise technology share 
###########################################
###########################################

technology_exposure <- results %>% 
  filter(ald_sector %in% c("automotive", "power")) %>% 
  summarise_technology_share(
    emission_factors_from = "plan_emission_factor", 
    production_from = "plan_alloc_wt_tech_prod", 
    technology_share_to = "technology_share"
  ) %>% 
  add_nice_names(
    names_from = "technology", 
    values_from = "technology_share", 
    names_prefix = "technology_share"
  ) %>% 
  distinct(!!!rlang::syms(output_columns))





