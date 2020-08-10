source("0_fund_analysis_functions.R")

# TODO sanity check technology alignment figures. 

project_name <- "daisy_test5"
project_location <- "/Users/vincentjerosch-herold/Dropbox (2° Investing)/PortCheck_v2/10_Projects/daisy_test5"

###########################################
###########################################
# ~ Load Data ~
###########################################
###########################################
load_fund_results(
  git_path = working_location,
  project_location = project_location
)

portfolio <- load_portfolio(project_location)

traffic_light_values <- prep_debt_economy()

write_csv(
  traffic_light_values, 
  path(project_location, "40_Results", "traffic_light_values", ext = "csv")
)

###########################################
###########################################
# calculate fund total value 
###########################################
###########################################

fund_total_value <- portfolio %>% 
  summarise_portfolio_value(portfolio_name)

fund_total_value <- fund_total_value %>% 
  mutate(
    fund_size_mio_euro = fund_size * 0.89089 / 1e6, # https://www.exchangerates.org.uk/USD-EUR-31_12_2019-exchange-rate-history.html
    currency = "EUR"
  )

###########################################
###########################################
# calculate asset type 
###########################################
###########################################

asset_type_exposure <- portfolio %>% 
  summarise_asset_type_exposure(portfolio_name)

# clean asset type exposure 
asset_type_exposure <- asset_type_exposure %>%
  mutate(
    asset_type_exposure = case_when(
      asset_type_exposure < 0 ~ 0, 
      asset_type_exposure > 1 ~ 1,
      TRUE ~ asset_type_exposure
    )
  )

asset_type_exposure <- asset_type_exposure %>%
  mutate(asset_type_exposure = round(asset_type_exposure, digits = 2))

# pivot wider 
asset_type_exposure <- asset_type_exposure %>% 
  pivot_wider(
    id_cols = c("portfolio_name"), 
    names_from = "asset_type", 
    values_from = "asset_type_exposure", 
    names_prefix = "asset_type_exposure_",
    values_fill = list(asset_type_exposure = 0)
  )

###########################################
###########################################
# complete fund matrix 
###########################################
###########################################

complete_fund_matrix <- general_fund_data %>% 
  transmute(
    fund_isin = as.character(fund_isin),
    fund_name = name,
    lipper_id
  ) %>% 
  distinct_all()

complete_fund_matrix <- complete_fund_matrix %>% 
  left_join(
    asset_type_exposure, 
    by = c("fund_isin" = "portfolio_name")
  )

portfolio <- load_portfolio(project_location)

security_mapped_sector_exposure <- portfolio %>% 
  summarise_group_share(
    id_cols = "investor_name",
    values_from = "value_usd",
    numerator_group = "security_mapped_sector", 
    denominator_group = "portfolio_name", 
    name_to = "sector_exposure", 
    na.rm = TRUE
  )

pacta_sector_exposure <- pacta_sector_exposure %>% 
  pivot_wider(
    names_from = "security_mapped_sector", 
    names_prefix = "sector_exposure_per_asset_type_", 
    values_from = "sector_exposure",
    values_fill = list(sector_exposure = 0)
  )


###########################################
###########################################
# paris alignment score 
###########################################
###########################################

results <- load_results(project_location) %>% 
  filter_unique_cross_sector_results()

paris_alignment_score <- results %>% 
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

results <- load_results(project_location) %>% 
  filter_unique_cross_sector_results()

paris_alignment_score <- results %>% 
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
# paris alignment score 
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
  summarise_by_group_weight(
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





