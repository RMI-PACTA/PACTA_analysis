source("0_fund_analysis_functions.R")

# TODO sanity check technology alignment figures. 

project_name <- "daisy_test5"
project_location <- "/Users/vincentjerosch-herold/Dropbox (2° Investing)/PortCheck_v2/10_Projects/daisy_test5"

###########################################
###########################################
# load data 
###########################################
###########################################

portfolio <- load_portfolio(project_location)

results <- load_results(project_location) %>% 
  filter_unique_cross_sector_results()

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
  )

# pivot wider 
asset_type_exposure <- asset_type_exposure %>% 
  mutate(asset_type_exposure = round(asset_type_exposure, digits = 2)) %>% 
  pivot_wider(
    id_cols = c("portfolio_name", "investor_name"), 
    names_from = "asset_type", 
    values_from = "asset_type_exposure", 
    names_prefix = "asset_type_exposure_",
    values_fill = list(asset_type_exposure = 0)
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
    numerator_group = "financial_sector", 
    denominator_group = "portfolio_name", 
    name_to = "pacta_sector_exposure", 
    na.rm = TRUE
  ) %>% 
  clean_and_pivot(
    values_from = "pacta_sector_exposure", 
    names_from = "financial_sector", 
    names_prefix = "sector_exposure",
    digits = 2
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
  summarise_portfolio_paris_alignment(
    portfolio,
    start_year = 2019,
    alignment_from = "trajectory_alignment"
  )

paris_alignment_build_out_score <- results %>% 
  calculate_build_out_alignment() %>% 
  add_nice_values(
    values_from = "build_out_alignment", 
    scale = 100,
    min = -100, 
    max = 100
  ) %>% 
  summarise_portfolio_paris_alignment(
    portfolio,
    start_year = 2019,
    alignment_from = "build_out_alignment"
  )

###########################################
###########################################
# technology exposure 
###########################################
###########################################

technology_exposure <- results %>% 
  summarise_group_share(
    id_cols = c("investor_name", "portfolio_name"),
    values_from = "plan_alloc_wt_tech_prod",
    numerator_group = "technology", 
    denominator_group = "ald_sector", 
    name_to = "technology_exposure", 
    na.rm = TRUE
  )

###########################################
###########################################
# paris alignment score 
###########################################
###########################################

results <- load_results(project_location)
portfolio <- load_portfolio(project_location)

results <- results %>% 
  inner_join(
    asset_type_exposure, 
    by = c("investor_name", "portfolio_name", "asset_type")
  )

results <- results %>% 
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
    max = 2009
  )

technology_alignment <- results %>% 
  summarise_group_weight(
    id_cols = c("investor_name", "portfolio_name", "ald_sector", "technology"), 
    weights_from = "asset_type_exposure",
    values_from = "technology_build_out_alignment",
    name_to = "technology_alignment",
    na.rm = TRUE
  )

country_exposure <- portfolio %>% 
  filter(!is.na(country_of_domicile)) %>% 
  summarise_group_share(
    id_cols = "investor_name",
    values_from = "value_usd",
    numerator_group = "country_of_domicile", 
    denominator_group = "portfolio_name", 
    name_to = "country_exposure", 
    na.rm = TRUE
  )

asset_type_exposure <- portfolio %>% 
  filter(asset_type %in% c("equity", "bonds")) %>% 
  summarise_group_share(
    id_cols = "investor_name",
    values_from = "value_usd",
    numerator_group = "asset_type", 
    denominator_group = "portfolio_name", 
    name_to = "asset_type_exposure", 
    na.rm = TRUE
  )

pacta_sector_exposure <- portfolio %>% 
  mutate(pacta_sector = if_else(security_mapped_sector != "other", "pacta_sector", "other_sector")) %>% 
  summarise_group_share(
    id_cols = "investor_name",
    values_from = "value_usd",
    numerator_group = "pacta_sector", 
    denominator_group = "portfolio_name", 
    name_to = "pacta_sector_exposure", 
    na.rm = TRUE
  )

###########################################
###########################################
# sensentive sector exposure
###########################################
###########################################

portfolio <- load_portfolio(project_location)

sensentive_exposures <- portfolio %>% 
  calculate_sensentive_exposures(
    id_cols = c("investor_name", "portfolio_name"), 
    market_value_from = "value_usd"
  ) %>% 
  summarise_group_share(
    id_cols = "investor_name", 
    numerator_group = "sensitive_sector", 
    denominator_group = "portfolio_name", 
    values_from = "adjusted_market_value", 
    name_to = "sensitive_sector_exposure", 
    na.rm = TRUE
  )

###########################################
###########################################
# weighted emissions factor 
###########################################
###########################################

results <- load_results(project_location)

tmp <- results %>% 
  filter(
    !is.nan(plan_emission_factor),
    scenario == "b2ds", 
    scenario_geography == "global", 
    year == 2020, 
    allocation == "portfolio_weight"
  ) %>% 
  calculate_group_share(
    id_cols = c("investor_name", "portfolio_name", "asset_type"), 
    numerator_group = "technology", 
    denominator_group = "ald_sector", 
    values_from = "plan_alloc_wt_tech_prod", 
    name_to = "technology_share", 
    na.rm = TRUE
  ) %>% 
  summarise_group_weight(
    id_cols = c("investor_name", "portfolio_name", "asset_type", "ald_sector"), 
    weights_from = "technology_share", 
    values_from = "plan_emission_factor"
  )





