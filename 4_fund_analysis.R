source("0_fund_analysis_functions_v2.R")

# TODO sanity check technology alignment figures. 
# TODO fund scores 
# TODO figure top function indicator, value style

project_name <- "daisy_test5"
project_location <- path(r2dii.utils::path_dropbox_2dii(), "PortCheck_v2", "10_Projects", project_name)
  
output_columns <- c("investor_name", "portfolio_name", "indicator", "value")

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
    name_to = "asset_type_exposure", 
    na.rm = TRUE
  ) %>% 
  add_nice_names(
    values_from = "asset_type_exposure",
    names_from = "asset_type",
    names_prefix = "exposure"
  )

# asset_type_exposure <- asset_type_exposure %>% 
#   pivot_wider_and_clean_names(
#     values_from = "asset_type_exposure", 
#     names_from = "asset_type", 
#     names_prefix = "exposure"
#   )

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
    numerator_group = "financial_sector", 
    denominator_group = "portfolio_name", 
    name_to = "pacta_sector_exposure", 
    na.rm = TRUE
  ) %>% 
  add_nice_names(
    values_from = "pacta_sector_exposure",
    names_from = "financial_sector",
    names_prefix = "sector_exposure"
  )

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
  mutate(score_type = "trajectory_alignment")

paris_alignment_build_out_score <- results %>% 
  calculate_build_out_alignment() %>% 
  add_nice_values(
    values_from = "build_out_alignment", 
    scale = 100,
    min = -100, 
    max = 100
  ) %>% 
  summarise_portfolio_paris_alignment(start_year = 2019, alignment_from = "build_out_alignment") %>% 
  mutate(score_type = "build_out_alignment")

alignment_scores <- paris_alignment_trajectory_score %>% 
  bind_rows(paris_alignment_build_out_score) %>% 
  add_nice_names(
    names_from = "score_type", 
    values_from = "alignment_portfolio"
  ) %>% 
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

technology_alignment <- technology_alignment %>% 
  summarise_group_weight(
    id_cols = c("investor_name", "portfolio_name", "ald_sector", "technology"), 
    weights_from = "asset_type_exposure",
    values_from = "tech_build_out_alignment",
    name_to = "technology_alignment",
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
    numerator_group = "company_name", 
    denominator_group = "portfolio_name"
  )

top_countries <- portfolio %>% 
  summarise_top_groups(
    n = 5, 
    id_cols = "investor_name",
    values_from = "value_usd",
    numerator_group = "country_of_domicile", 
    denominator_group = "portfolio_name"
  )

top_currencies <- portfolio %>% 
  summarise_top_groups(
    n = 5, 
    id_cols = "investor_name",
    values_from = "value_usd",
    numerator_group = "currency", 
    denominator_group = "portfolio_name"
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
    name_to = "sensitive_sector_exposure", 
    na.rm = TRUE
  )

sensentive_exposures <- sensentive_exposures %>%
  filter(sensitive_sector != "other") %>% 
  add_nice_names(
    names_from = "sensitive_sector", 
    values_from = "sensitive_sector_exposure", 
    names_prefix = "sector_exposure"
  ) %>% 
  distinct(!!!rlang::syms(output_columns))

sensentive_exposures <- sensentive_exposures %>% 
  pivot_wider(
    names_from = "indicator", 
    values_from = "value"
  )

###########################################
###########################################
# weighted emissions factor 
###########################################
###########################################

emission_factors <- results %>% 
  summarise_emission_factor(names_prefix = "sector_co2_intensity")

###########################################
###########################################
# summarise technology share 
###########################################
###########################################

technology_exposure_auto <- results %>% 
  filter(ald_sector == "automotive") %>% 
  summarise_technology_share(names_prefix = "technology_exposure") 

technology_exposure_power <- results %>% 
  filter(ald_sector == "power") %>% 
  summarise_technology_share(names_prefix = "technology_exposure") 




