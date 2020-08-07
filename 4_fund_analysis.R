source("0_fund_analysis_functions.R")

###########################################
###########################################
# ~ Load Data ~
###########################################
###########################################
load_fund_results(
  git_path = working_location,
  project_location = project_location
)

audit_file <- load_pacta_portfolio(project_location)

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


sector_exposure_per_asset_type_wide <- sector_exposure_per_asset_type %>% 
  pivot_wider(
    names_from = "security_mapped_sector", 
    names_prefix = "sector_exposure_per_asset_type_", 
    values_from = "sector_exposure",
    values_fill = list(sector_exposure = 0)
  )

sector_exposure_per_asset_type_view <- sector_exposure_per_asset_type_wide %>% 
  mutate(asset_type = tolower(asset_type)) %>% 
  distinct(
    portfolio_name, 
    sector_exposure_per_asset_type_power, 
    asset_type
  )

pacta_audit %>% 
  summarise_pacta_sector_exposure(portfolio_name)

