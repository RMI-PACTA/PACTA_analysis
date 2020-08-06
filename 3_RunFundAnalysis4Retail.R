# 3_RunFundAnalysis4Retail.R

# This script prepares the result matrix for fund assessments for retail projects
# It gathers the relevant datasets and formats it in a way to have a single output matrix at fund level
# file parameter naming styles:
# pacta_portfolio_results
# pacta_companies_results
# processed_portfolio

# The user can select the indicators that he is interested in via a parameter file 
#TODO: ask Mauro what his thoughts on this are - input must define a) the indicators and 2) the type of output (e.g. bool vs numeric) 
# The output will be the result matrix with the users selection 
# The function can be used to prepare data for all retail projects, e.g. the NKI FundMatrix, LITA data, data for meta analysis of funds and fund reports 

library(scales)
library(reshape2)
library(tidyverse)
library(here)
library(fs)

# ~ PARAMETERS THAT CAN BE SET FOR THE FUND ANALYSIS
# Fund Size used as a denominator
# coverage threshold to filter out funds that have a good enough coverage
# TBD: Benchmark scenario? 
# TBD: Benchmark market for e.g. traffic light system?
coverage_threshold = 0.70

# TODO: Ask Mauro how to install a library from a PR or merge the Single Indicator PR into the master 
# library(r2dii.analysis)

# Need to run 1_PortCheckInitialisation.R

#TODO: collect all functions in a script "0_FundAnalysis4Retail_functions.R"
source("0_FundAnalysis4Retail_functions.R")

# ~ Load Data ~
load_fund_results(
  git_path = GIT.PATH,
  project_location = PROJ.LOCATION
)

# ~ Meta analysis ~
# 1) Meta fond ergebnisse ("Meta Portfolio") f�r alle Indikatoren
# 2) Fond universe: Distribution charts f�r alle Indikatoren evtl zusammen mit Meta-charts
# 3) Firmen Ergennisse (Company Impact tracking) /Meta fond ergebnisse als % der Firmen f�r ausgew�hlte indikatoren - both client view (full company level production) as well as exposure view
#   a) technology share: RE, EV, Coal, Nuclear
#   b) climabte alignment: RE, EV, 
#   c) build out: coal power, oil&gas, coal mining
# 4) Expositions�nderungen (Benchmark changes)
#   a) Asset type
#   b) Companies expositionen

# 3 Company share
company_share_data <- input_pacta_company_results %>% 
  filter(
    portfolio_name == "Meta Portfolio",
    year %in% c(START.YEAR,START.YEAR+5),
    allocation == "portfolio_weight")

identify_range <- function(value, range_size){
  value <- floor(value/range_size) * range_size 
}

company_share_data <- company_share_data %>% mutate(
  tech_share_range = identify_range(value = plan_tech_prod / plan_sec_prod * 100, 10)
)

tech_share_distribution <- company_share_data %>% filter(year == START.YEAR+5) %>% 
  select(id, ald_sector, technology, port_weight, plan_sec_prod, tech_share_range) %>% distinct() %>% 
  group_by(ald_sector, technology, tech_share_range) %>% 
  summarise(
    financial_exposure = sum(port_weight,na.rm = T),
    client_exposure = sum(plan_sec_prod),
  ) %>% 
  group_by(ald_sector, technology) %>% 
  mutate(
    financial_exposure_sector = sum(financial_exposure,na.rm = T),
    client_exposure_sector = sum(client_exposure,na.rm = T)
  ) %>% ungroup() %>% 
  mutate(
    financial_exposure_perc = financial_exposure / financial_exposure_sector,
    client_exposure_perc = client_exposure / client_exposure_sector
  )

write.csv(tech_share_distribution,  paste0(PROJ.LOCATION, "40_Results/", Project.Name, "Meta_results_company_distribution_sample.csv"), row.names = F, na = "")


# ~ calculation of the corporate economy to get the traffic light values
# TODO: use the corporate economy based on all ALD, currently on the dept universe is used as the entire corporate economy wasn't formated appropriately
# corporate_economy_data <- read.csv(file = paste0(DATA.PATH, "/06_DataStore/",DATASTORE.TIMESTAMP, "/", ALD.TIMESTAMP,"/masterdata_full.csv"))
# equity_economy <- read.csv(file = paste0(DATA.PATH, "/06_DataStore/",DATASTORE.TIMESTAMP, "/", ALD.TIMESTAMP,"/masterdata_ownership.csv"))
browntechs <- c("Oil","Gas","Coal","CoalCap","GasCap","ICE")

debt_economy <- read_rds(paste0(DATA.PATH, "/07_AnalysisInputs/2019Q4_250220/bonds_ald_scenario_long.rda")) %>% 
  filter(
    (ald_sector == "Power" & scenario_geography == "GlobalAggregate") | (ald_sector != "Power" & scenario_geography == "Global"),
    year %in% c(START.YEAR,START.YEAR+5),
    !is.na(scenario)
  ) %>%  
  select(scenario, ald_sector, technology, year, 
         plan_tech_prod, plan_emission_factor, plan_build_out, plan_sector_prod,
         scen_tech_prod, scen_emission_factor, scen_build_out, scen_SecProd)

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
    plan_build_out = plan_tech_prod - sum(if_else(year == START.YEAR,plan_tech_prod,0),na.rm = T),    
    scen_build_out = scen_tech_prod - sum(if_else(year == START.YEAR,scen_tech_prod,0),na.rm = T)
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
    
  ) %>% ungroup()

economy_sector_co2_intensity_data <- debt_economy %>% 
  filter(year == START.YEAR) %>% 
  transmute(ald_sector, 
            traffic_light_yellow = plan_sec_emissions_factor) %>%
  distinct()

# ~ traffic light system ~
traffic_light_sector_co2_intensity_data <- debt_economy %>% 
  filter(year == START.YEAR+5, scenario %in% c("B2DS","SBTI")) %>% 
  transmute(ald_sector, 
            traffic_light_green = scen_sec_emissions_factor) %>%
  distinct() %>% full_join(economy_sector_co2_intensity_data, by = "ald_sector")

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
  filter(scenario %in% c("B2DS","SBTI"), year == START.YEAR+5, ald_sector %in% c("power","automotive"),!is.na(plan_tech_share)) %>% 
  transmute(
    indicator_name = paste0("technology_share_",str_remove(technology,"cap")),
    traffic_light_yellow = plan_tech_share * 100, 
    traffic_light_green = scen_tech_share * 100) %>% distinct()

traffic_light_technology_climate_alignment <- debt_economy %>% 
  filter(
    year == START.YEAR+5,
    technology %in% c("renewablescap", "electric"),
    scenario == "B2DS") %>% 
  mutate(
    technology_climate_alignment = plan_build_out / scen_build_out * 100
  ) %>% 
  transmute(
    indicator_name = paste0("climate_alignment_", str_remove(technology,"cap")),
    traffic_light_yellow = round(if_else(technology_climate_alignment>200,200.00,technology_climate_alignment),2),
    traffic_light_green = 100
  ) %>% distinct() 

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

write.csv(traffic_light_values, paste0(PROJ.LOCATION, "40_Results/traffic_light_values.csv"), row.names = F, na = "")

# TODO: write small function that creates meta statistic for the selected indicators: a) average and b) min-max values

# ~ General Fund Indicators from MS ~
# Fond ISIN # general information
# Fond Name # general information
# Fond Size # financial/general information
# Fond Coverage # general information
# TODO: add financial indicators to this list - to be done in the fund processing functions


# ~ Asset Type Exposure calculation ~
# Financial Asset Type exposure # general information
# File: Audit-File
# function identify sov. bonds, corp. bonds, equity, others and NA

# TODO: implement currency conversion, either find function or write it to translate the MarketValue to ValueUSD
# currency_conversion <- function(input_data, currency_data, target_currency = "USD"){}

# take only essential columns 

# calculate_asset_type_exposure <- function()

# flag asset types 
asset_type_exposure <- pacta_audit %>% 
  mutate(
    asset_type = case_when(
      security_bics_subgroup %in% c("Sovereign","Sovereign Agency", "Sovereigns") ~ "Sovereign",
      security_type %in% c("Sovereign Debt","Sovereign Agency Debt", "Government inflation linked Bonds") ~ "Sovereign",
      flag == "Holding not in Bloomberg database" ~ "no_data_available",
      TRUE ~ asset_type
    )
  )

# first calculate total fund market value  
fund_total_value <- asset_type_exposure %>% 
  group_by(portfolio_name) %>% 
  summarise(fund_size_covered = sum(value_usd, na.rm = TRUE)) %>% 
  ungroup()

# summarize total asset type value 
asset_type_exposure <- asset_type_exposure %>% 
  group_by(
    portfolio_name, 
    asset_type
  ) %>% 
  summarize(asset_type_value = sum(value_usd, na.rm = TRUE))

# calculate total fund value and asset type exposure 
asset_type_exposure <- asset_type_exposure %>%
  left_join(
    fund_total_value, 
    by = "portfolio_name"
  ) %>% 
  mutate(asset_type_exposure = asset_type_value / fund_size_covered)

# clean asset type exposure 
asset_type_exposure <- asset_type_exposure %>%
  mutate(
    asset_type_exposure = case_when(
      asset_type_exposure < 0 ~ 0, 
      asset_type_exposure > 1 ~ 1,
      TRUE ~ asset_type_exposure
    ),
    asset_type_exposure = round(asset_type_exposure, digits = 2)
  )

# pivot wider 
asset_type_exposure_wide <- asset_type_exposure %>% 
  pivot_wider(
    id_cols = c("portfolio_name", "fund_size_covered"), 
    names_from = "asset_type", 
    values_from = "asset_type_exposure", 
    names_prefix = "asset_type_exposure_",
    values_fill = list(asset_type_exposure = 0)
  )

# general_fund_total_value <- project_general_fund_data %>% 
#   distinct(
#     isin, 
#     total_market_value
#   )

# fund_total_value_check <- fund_total_value %>% 
#   full_join(
#     general_fund_total_value, 
#     by = c("portfolio_name" = "isin"), 
#     suffix = c("_port", "_ms")
#   )

# create complete fund matrix 
complete_fund_matrix <- project_general_fund_data %>% 
  transmute(
    fund_isin = as.character(fund_isin),
    fund_name = name,
    lipper_id
  ) %>% 
  distinct_all()

complete_fund_matrix <- complete_fund_matrix %>% 
  left_join(
    asset_type_exposure_wide, 
    by = c("fund_isin" = "portfolio_name")
  )

# TODO use currency file to calculate fund size in mio Euro
fund_value_mio_euro <- pacta_audit %>% 
  group_by(portfolio_name) %>% 
  summarise(fund_size = sum(value_usd, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(
    fund_size_mio_euro = fund_size * 0.89089 / 1e6, # https://www.exchangerates.org.uk/USD-EUR-31_12_2019-exchange-rate-history.html
    currency = "EUR"
  )

# clean view 


# join to euro fund value 
complete_fund_matrix <- complete_fund_matrix %>% 
  left_join(
    fund_value_mio_euro, 
    by = c("fund_isin" = "portfolio_name")
  )

# Calculate sector exposure per asset type 
sector_exposure_per_asset_type <- pacta_audit %>%
  filter(
    security_mapped_sector != "other", 
    security_mapped_sector == financial_sector | (security_mapped_sector %in% c("steel","cement") & financial_sector == "cement&steel")# , security_mapped_sector == financial_sector
  ) %>% 
  mutate(security_mapped_sector = tolower(security_mapped_sector))

sector_exposure_per_asset_type <- sector_exposure_per_asset_type %>% 
  group_by(
    portfolio_name,
    security_mapped_sector, 
    asset_type
  ) %>%
  summarise(sector_exposure = sum(value_usd, na.rm = TRUE)) %>% 
  ungroup()

fund_size_covered <- complete_fund_matrix %>% 
  distinct(
    fund_isin, 
    fund_size_covered
  ) %>% 
  rename(portfolio_name = fund_isin)

sector_exposure_per_asset_type <- sector_exposure_per_asset_type %>% 
  left_join(
    fund_size_covered, 
    by = c("portfolio_name")
  )

sector_exposure_per_asset_type <- sector_exposure_per_asset_type %>% 
  mutate(
    sector_exposure = sector_exposure / fund_size_covered,
    security_mapped_sector = if_else(security_mapped_sector == "coal","coal_mining", security_mapped_sector)
  ) %>% 
  select(-fund_size_covered)

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

# Paris Alignment Single Indicator Calculation ----------------------------

# ~ Paris Alignment Single Indicator (InfluenceMap Score) ~
# TODO: load single indicators functions from r2dii.analysis
# InfluenceMap Paris alignment score # climate indicator
# Sector exposure to climate-relevant sectors with scenario analysis # climate indicator
# TODO: Solve shipping sector issue (reflect the AB and GH overexposure as InfluenceMap does)
portfolio_climate_alignment <- input_pacta_portfolio_results %>% 
  filter(
    scenario == "B2DS",
    allocation == "portfolio_weight",
    year == START.YEAR + 5
  ) %>% 
  mutate(
    trajectory_alignment = case_when(
      trajectory_alignment > 1 ~ 1,
      trajectory_alignment < -1 ~ -1,
      TRUE ~ trajectory_alignment
    )
  )

portfolio_climate_alignment <- influencemap_weighting_methodology(
  pacta_portfolio_results = portfolio_climate_alignment,
  processed_portfolio = pacta_audit,
  metric_col = "trajectory_alignment"
)

portfolio_climate_alignment <- portfolio_climate_alignment %>% 
  distinct(
    portfolio_name,
    metric_port
  )

portfolio_climate_alignment <- portfolio_climate_alignment %>% 
  mutate(
    climate_alignment_portfolio_aggregate = case_when(
      metric_port > 1 ~ 100,
      metric_port < -1 ~ -100,
      TRUE ~ metric_port*100
    )
  ) %>% 
  select(-metric_port)

complete_fund_matrix <- complete_fund_matrix %>% 
  left_join(
    portfolio_climate_alignment,
    by = c("fund_isin" = "portfolio_name")
  )


# Paris Alignment Single Indicator Calculation based on build out  --------

# ~ Paris Alignment Single Indicator (InfluenceMap Score) using build out only~
portfolio_build_out_climate_alignment <- input_pacta_portfolio_results %>% 
  filter(
    scenario == "B2DS",
    allocation == "portfolio_weight")  %>% 
  select(
    investor_name, 
    portfolio_name, 
    asset_type,
    year, 
    ald_sector, 
    technology, 
    plan_alloc_wt_tech_prod, 
    scen_alloc_wt_tech_prod, 
    plan_sec_emissions_factor, 
    scen_sec_emissions_factor
  ) %>% 
  filter(plan_alloc_wt_tech_prod != 0)

high_carbon_tech_list <- c("Oil","Gas","Coal","CoalCap","GasCap","ICE", "OilCap")

portfolio_build_out_climate_alignment <- portfolio_build_out_climate_alignment %>%
  group_by(
    investor_name, portfolio_name,asset_type, ald_sector, technology
  ) %>% 
  mutate(
    reference_value = sum(if_else(year == START.YEAR,plan_alloc_wt_tech_prod,0)),
    
    build_out_deviation_tech = case_when(
      !technology %in% tolower(high_carbon_tech_list) & (scen_alloc_wt_tech_prod - reference_value) == 0 & plan_alloc_wt_tech_prod > 0 ~ 1,
      !technology %in% tolower(high_carbon_tech_list) ~ (plan_alloc_wt_tech_prod - scen_alloc_wt_tech_prod) / (scen_alloc_wt_tech_prod - reference_value),
      technology %in% tolower(high_carbon_tech_list) & (scen_alloc_wt_tech_prod - reference_value) == 0 & (plan_alloc_wt_tech_prod > reference_value) ~ 1,
      technology %in% tolower(high_carbon_tech_list) & (scen_alloc_wt_tech_prod - reference_value) == 0 & (plan_alloc_wt_tech_prod < reference_value) ~ -1,
      # technology %in% tolower(high_carbon_tech_list) & (scen_alloc_wt_tech_prod > reference_value) ~ 1 + (plan_alloc_wt_tech_prod - scen_alloc_wt_tech_prod) / abs(scen_alloc_wt_tech_prod - reference_value),
      # T ~ (plan_alloc_wt_tech_prod - scen_alloc_wt_tech_prod) / abs(scen_alloc_wt_tech_prod - reference_value)),
      technology %in% tolower(high_carbon_tech_list) & plan_alloc_wt_tech_prod > scen_alloc_wt_tech_prod ~ (plan_alloc_wt_tech_prod - scen_alloc_wt_tech_prod) / abs(scen_alloc_wt_tech_prod - reference_value),
      T ~ -(plan_alloc_wt_tech_prod - scen_alloc_wt_tech_prod) / (scen_alloc_wt_tech_prod - reference_value)),
        
    build_out_deviation_sec = (plan_sec_emissions_factor - scen_sec_emissions_factor) / scen_sec_emissions_factor,
    build_out_deviation_sec = if_else(scen_sec_emissions_factor == 0, if_else(plan_sec_emissions_factor == 0, 0, -1), build_out_deviation_sec),
    
    build_out_deviation = if_else(ald_sector %in% tolower(OTHER.SECTOR.LIST),build_out_deviation_sec, build_out_deviation_tech),
    
    build_out_alignment = case_when(
      ald_sector %in% tolower(OTHER.SECTOR.LIST) ~ -build_out_deviation,
      technology %in% tolower(high_carbon_tech_list) ~ -build_out_deviation,
      T ~ build_out_deviation
    )) %>% 
    filter(
      year == START.YEAR + 5
    ) 

portfolio_build_out_climate_alignment <- portfolio_build_out_climate_alignment %>%
  mutate(
    build_out_alignment = case_when(
      build_out_alignment > 1 ~ 1,
      build_out_alignment < -1 ~ -1,
      T ~ build_out_alignment
    )
  )

portfolio_build_out_climate_alignment <- influencemap_weighting_methodology(
  pacta_portfolio_results = portfolio_build_out_climate_alignment,
  processed_portfolio = pacta_audit,
  metric_col = "build_out_alignment"
)

portfolio_build_out_climate_alignment <- portfolio_build_out_climate_alignment %>% 
  distinct(
    portfolio_name,
    metric_port
  )

portfolio_build_out_climate_alignment <- portfolio_build_out_climate_alignment %>% 
  mutate(
    build_out_climate_alignment_portfolio_aggregate = case_when(
      metric_port > 1 ~ 100,
      metric_port < -1 ~ -100,
      TRUE ~ metric_port*100
    )
  ) %>% 
  select(-metric_port)

complete_fund_matrix <- complete_fund_matrix %>% 
  left_join(
    portfolio_build_out_climate_alignment,
    by = c("fund_isin" = "portfolio_name")
  )

# ~ Sector Exposure to sectors in the Paris Alignment Single Indicator (InfluenceMap Score) ~
#finding exposure to climate relevant sectors 
climate_rel_sector_exposure <- mapped_sector_exposure(
  input_audit = pacta_audit,
  group_vars = "portfolio_name",
  fund_size_data = fund_size_covered,
  fund_size_indicator = "fund_size_covered",
  sector_list = c("power", "aviation", "oil&gas", "steel", "coal", "cement", "automotive")
  )

complete_fund_matrix <- complete_fund_matrix %>% 
  left_join(
    climate_rel_sector_exposure, 
    by = c("fund_isin" = "portfolio_name")
  )

# ~ Average CO2-Intensity of climate relevant sectors ~
#   power generation # climate indicator
#   automotive fleet # climate indicator
#   steel production # climate indicator
#   cement production # climate indicator

sector_co2_intensity <- input_pacta_portfolio_results %>% 
  filter(
    year == START.YEAR, 
    allocation == "portfolio_weight", 
    !is.na(plan_sec_emissions_factor)
  ) %>% 
  distinct(
    portfolio_name, 
    ald_sector, 
    plan_sec_emissions_factor, 
    asset_type, 
    plan_sec_carsten
  ) 

sector_co2_intensity <- sector_co2_intensity %>% 
  inner_join(
    asset_type_exposure, 
    by = c("portfolio_name" ,"asset_type")
  )

sector_co2_intensity <- sector_co2_intensity %>%  
  left_join(
    sector_exposure_per_asset_type,
    by = c("portfolio_name" ,"asset_type", "ald_sector" = "security_mapped_sector")
  )

sector_co2_intensity <- sector_co2_intensity %>% 
  group_by(
    portfolio_name, 
    ald_sector
  ) %>% 
  mutate(sector_co2_intensity = weighted.mean(plan_sec_emissions_factor, asset_type_exposure * sector_exposure, na.rm = T)
  ) %>% 
  ungroup()

sector_co2_intensity <- sector_co2_intensity %>% 
  distinct(
    portfolio_name, 
    ald_sector,
    sector_co2_intensity
  ) 

sector_co2_intensity <- sector_co2_intensity %>% 
  mutate(sector_co2_intensity = if_else(ald_sector %in% c("aviation", "automotive", "shipping"), sector_co2_intensity * 1e6, sector_co2_intensity))

sector_co2_intensity <- sector_co2_intensity %>% 
  mutate(sector_co2_intensity = signif(sector_co2_intensity,digits = 3))

sector_co2_intensity_wide <- sector_co2_intensity %>% 
  pivot_wider(
    id_cols = "portfolio_name",
    names_from = "ald_sector",
    names_prefix = "sector_co2_intensity_",
    values_from = "sector_co2_intensity"
  )

complete_fund_matrix <- complete_fund_matrix %>% 
  left_join(
    sector_co2_intensity_wide, 
    by = c("fund_isin" = "portfolio_name")
  )

# ~ Sector & Technology Exposure ~
# TODO: Use revenue split, discuss with Clare & Vincent, 1) which data to use and 2) which functions to use - align both workstreams (Clare's and Vincent's) into one
# Sector exposure to specific climate-relevant or environmental or socially sensitive sectors:
#   coal mining # climate indicator
#   power sector # climate indicator
#   nuclear incl Uranium production # environmnetal indicator
#   palm-oil sector # environmental indicator # bool
#   weapons sector # social indicator # bool
#   addictive_substances sector # social indicator # bool

sector_exposure <- pacta_audit %>%
  mutate(security_mapped_sector = tolower(security_mapped_sector)) %>% 
  filter(
    security_mapped_sector != "other", 
    has_ald_in_fin_sector == TRUE,
    security_mapped_sector == financial_sector | (security_mapped_sector %in% c("steel","cement") & financial_sector == "cement&steel")# , security_mapped_sector == financial_sector
  )

sector_exposure <- sector_exposure %>% 
  group_by(
    portfolio_name,
    security_mapped_sector
  ) %>%
  summarise(sector_value = sum(value_usd, na.rm = TRUE)) %>% 
  ungroup()

sector_exposure <- sector_exposure %>% 
  left_join(
    fund_size_covered, 
    by = c("portfolio_name")
  )

# calculate sector exposure 
sector_exposure <- sector_exposure %>% 
  mutate(sector_exposure = sector_value / fund_size_covered) %>% 
  distinct(
    portfolio_name, 
    security_mapped_sector, 
    sector_exposure
  )

# cleaning outputs 
sector_exposure <- sector_exposure %>% 
  mutate(security_mapped_sector = if_else(security_mapped_sector == "coal","coal_mining", security_mapped_sector))

sector_exposure_wide <- sector_exposure  %>% 
  pivot_wider(
    names_from = "security_mapped_sector", 
    names_prefix = "sector_exposure_", 
    values_from = "sector_exposure",
    values_fill = list(sector_exposure = 0)
  ) 

#TODO: use fund_size from general fund data to calculate the sensitive sector exposure

sensitive_sector_list <- setdiff(unique(sensitive_sectors$sector),unique(tolower(revenue_data$sector)))

sensitive_sector_exposure <- calculate_sensitive_sector_exposure(
  input_audit = pacta_audit,
  sector_list = sensitive_sector_list, #TODO make this nices once the revenue data is clean!
  sensitive_sectors_data = sensitive_sectors,
  flag = FALSE,
  fund_size_data = fund_size_covered,
  fund_size_indicator = "fund_size_covered"
)

exposure_data <- sensitive_sector_exposure %>% 
  select(-investor_name) %>% 
  full_join(
    sector_exposure_wide, 
    by = "portfolio_name"
  ) 

summarize_multiple_sensitive_sectors <- function(
  input_data = sensitive_sector_exposure, 
  summarize_sector_list, 
  sector_output_name, 
  group_vars = "portfolio_name"
) {
  
  sector_list_exposure <- input_data %>% 
    pivot_longer(
      cols = starts_with("sector_exposure_"), 
      names_to = "sector", 
      names_prefix = "sector_exposure_", 
      values_to = "sector_exposure"
    )
  
  sector_list_exposure <- sector_list_exposure %>% 
    filter(sector %in% summarize_sector_list) %>% 
    group_by(!!!syms(group_vars)) %>% 
    summarise(sector_exposure = sum(sector_exposure,na.rm = TRUE)) 
  
  sector_list_exposure %>% 
    set_names(
      group_vars,
      paste0("sector_exposure_", sector_output_name)
    ) %>% 
    ungroup()
}

addictive_substances_exposure <- summarize_multiple_sensitive_sectors(
  input_data = exposure_data, 
  summarize_sector_list = c("tobacco", "tobacco_products", "alcoholic_beverages"),
  sector_output_name = "addictive_substances",
  group_vars = "portfolio_name"
)

weapons_exposure <- summarize_multiple_sensitive_sectors(
  input_data = exposure_data, 
  summarize_sector_list = c("defense_primes"),
  sector_output_name = "weapons",
  group_vars = "portfolio_name"
)

palm_oil_exposure <- summarize_multiple_sensitive_sectors(
  input_data = exposure_data, 
  summarize_sector_list = c("palm_oil_farming", "palm_oil_processing"),
  sector_output_name = "palm_oil",
  group_vars = "portfolio_name"
)

meat_exposure <- summarize_multiple_sensitive_sectors(
  input_data = exposure_data, 
  summarize_sector_list = c("animal_production_&_process","animal_feed", "meat_products"),
  sector_output_name = "meat",
  group_vars = "portfolio_name"
)

exposure_data <- exposure_data %>% 
  full_join(addictive_substances_exposure, by = "portfolio_name") %>% 
  full_join(weapons_exposure, by = "portfolio_name") %>%  
  full_join(palm_oil_exposure, by = "portfolio_name") %>% 
  full_join(meat_exposure, by = "portfolio_name") 

# Technology exposure to specific climate-relevant Technologies:
#   coal power generation # climate indicator
#   renewables power generation # climate indicator
# TODO: review this function, prioritize ALD over revenue data!! 
# technology_exposure <- calculate_technology_sector_exposure(
#   input_results = input_pacta_company_results,
#   input_audit = pacta_audit,
#   technology_list = c("CoalCap", "NuclearCap")
# )
# this funciton is solely to understand the portfolio weight (carstens metric * asset type weight) of the technologies.
# only relevant for coal and nuclear (uran exposure flag and coal exposure flag)
# the function is much to complex for what is needed at the moment, but can be used at a later stage potentially. 
# for now re-caclulate the tech exposure in a separate function and keep this one untouched

# technology_exposure =  sector_exposure * technology_share (weighted by asset type)
technology_exposure <- input_pacta_portfolio_results %>% 
  filter(
    year == START.YEAR, 
    technology %in% c("coalcap", "nuclearcap"),
    !is.na(plan_tech_share),
    plan_sec_carsten!=0
  ) %>% 
  distinct(
    portfolio_name, 
    asset_type, 
    ald_sector,
    technology, 
    plan_tech_share, 
    plan_sec_carsten
  )

asset_type_exposure_view <- asset_type_exposure  %>% 
  distinct(
    portfolio_name,
    asset_type,
    asset_type_exposure
  )

technology_exposure <- technology_exposure %>% 
  left_join(
    asset_type_exposure_view, 
    by = c("portfolio_name", "asset_type")
  )

technology_exposure <- technology_exposure %>% 
  left_join(
    sector_exposure_per_asset_type_view,
    by = c("portfolio_name" ,"asset_type")
  )

technology_exposure <- technology_exposure %>% 
  mutate(technology_exposure = plan_tech_share * asset_type_exposure * sector_exposure_per_asset_type_power)

technology_exposure <- technology_exposure %>% 
  group_by(
    portfolio_name, 
    ald_sector, 
    technology
  ) %>% 
  summarize(
    technology_exposure = sum(technology_exposure,na.rm = T)
  ) %>% 
  ungroup()

technology_exposure <- technology_exposure %>% 
  transmute(
    portfolio_name, 
    technology = str_remove(technology,"cap"), 
    technology_exposure) %>% 
  distinct_all()

technology_exposure <- technology_exposure %>% 
  pivot_wider(
    id_cols = "portfolio_name",
    names_from = technology,
    names_prefix = "technology_exposure_",
    values_from = technology_exposure
  )

exposure_data <- exposure_data %>%  
  full_join(
    technology_exposure, 
    by = c("portfolio_name")
  )

# Combine sector and technology exposures into one indicator
summarize_exposure_to_sectors_and_technology <- function(
  input_data = exposure_data, 
  summarize_sector_list, 
  summarize_technology_list, 
  output_name, 
  group_vars = "portfolio_name"
){
  
  sector_list_exposure <- input_data %>% 
    select(
      portfolio_name, 
      starts_with("sector_exposure_")
    )
  
  sector_list_exposure <- sector_list_exposure %>% 
    pivot_longer(
      cols = starts_with("sector_exposure_"), 
      names_to = "sector", 
      names_prefix = "sector_exposure_", 
      values_to = "activity_exposure"
    ) %>% 
    filter(sector %in% summarize_sector_list) 
  
  technology_list_exposure <- input_data %>% 
    select(
      portfolio_name,
      starts_with("technology_exposure_")
    )
  
  technology_list_exposure <- technology_list_exposure %>% 
    pivot_longer(
      cols = starts_with("technology_exposure_"), 
      names_to = "sector", 
      names_prefix = "technology_exposure_", 
      values_to = "activity_exposure"
    ) %>% 
    filter(sector %in% summarize_technology_list) 
  
  exposure <- bind_rows(
    technology_list_exposure,
    sector_list_exposure
  )
  
  exposure <- exposure %>% 
    group_by(!!!syms(group_vars)) %>% 
    summarise(activity_exposure = sum(activity_exposure, na.rm = TRUE))
  
  exposure %>% 
    set_names(
      group_vars,
      paste0("activity_exposure_",output_name)
    )
}

# combined uran and nuclear exposure
uran_exposure <- summarize_exposure_to_sectors_and_technology(
  input_data = exposure_data, 
  summarize_sector_list = "uranium",
  summarize_technology_list = "nuclear",
  output_name = "uran",
  group_vars = "portfolio_name"
)

exposure_data <- exposure_data %>% 
  full_join(
    uran_exposure, 
    by = "portfolio_name"
  )

# combined coal mining and coal power exposure 
coal_exposure <- summarize_exposure_to_sectors_and_technology(
  input_data = exposure_data, 
  summarize_sector_list = c("coal_mining", "coal_support_services"),
  summarize_technology_list = "coal",
  output_name = "coal",
  group_vars = "portfolio_name"
)

exposure_data <- exposure_data %>% 
  full_join(
    coal_exposure, 
    by = "portfolio_name"
  )

exposure_data <- exposure_data %>% 
  mutate_at(
    .vars = c(setdiff(names(exposure_data), "portfolio_name")),
    .funs = ~if_else(is.na(.), 0, .)
  )

complete_fund_matrix <- complete_fund_matrix %>% 
  left_join(
    exposure_data, 
    by = c("fund_isin" = "portfolio_name")
  )

# ~ Technology Share in specific climate relevant sectors with green alternatives ~
# calculate technology share for PACTA sectors (automotive and power)
 

technology_share_data <- input_pacta_portfolio_results %>% 
  filter(year == START.YEAR+5, allocation == "portfolio_weight", ald_sector %in% c("power","automotive"), plan_alloc_wt_sec_prod != 0) %>% 
  select(portfolio_name, ald_sector, technology, plan_tech_share, asset_type) %>% distinct()

technology_share_data <- technology_share_data %>% 
  left_join(sector_exposure_per_asset_type,  by = c("portfolio_name","asset_type", "ald_sector" = "security_mapped_sector")) %>% 
  left_join(asset_type_exposure, by = c("portfolio_name","asset_type"))

technology_share_data <- technology_share_data %>% 
  group_by(portfolio_name, ald_sector, technology) %>% 
  mutate(
    technology_share = weighted.mean(plan_tech_share, asset_type_exposure * sector_exposure, na.rm = T)
  ) %>% ungroup()

technology_share_data_auto <- technology_share_data %>% 
  filter(ald_sector == "automotive") %>% 
  transmute(portfolio_name, technology = str_remove(technology,"cap"), technology_share) %>% distinct() %>%  
  pivot_wider(
    id_cols = "portfolio_name",
    names_from = technology,
    names_prefix = "technology_share_",
    values_from = technology_share,
    values_fill = list(technology_share = 0)
  )

technology_share_data_power <- technology_share_data %>% 
  filter(ald_sector == "power") %>% 
  transmute(portfolio_name, technology = str_remove(technology,"cap"), technology_share) %>% distinct() %>%  
  pivot_wider(
    id_cols = "portfolio_name",
    names_from = technology,
    names_prefix = "technology_share_",
    values_from = technology_share,
    values_fill = list(technology_share = 0)
  )


complete_fund_matrix <- complete_fund_matrix %>% left_join(technology_share_data_auto, by = c("fund_isin" = "portfolio_name"))
complete_fund_matrix <- complete_fund_matrix %>% left_join(technology_share_data_power, by = c("fund_isin" = "portfolio_name"))

# ~ Technology build out scenario compatibility for green, climate-relevant technologies ~
# Scenario compatibility:
#   RE build out plans # climate indicator
calculate_technology_climate_alignment <- function(pacta_input_portfolio = input_pacta_portfolio_results, 
                                                   start_year = START.YEAR, 
                                                   technology_selection = "renewablescap", 
                                                   scenario_selection = "B2DS"){
  
  technology_climate_alignment_data <- pacta_input_portfolio %>% 
    filter(
      year %in% c(start_year, start_year+5),
      allocation == "portfolio_weight",
      technology == technology_selection,
      scenario == scenario_selection
    ) %>% 
    select(
      portfolio_name, plan_alloc_wt_tech_prod, scen_alloc_wt_tech_prod, year, asset_type, plan_sec_carsten
    ) %>% 
    mutate(
      plan_sec_carsten = round(plan_sec_carsten,8)
    )
  
  technology_climate_alignment_data <- technology_climate_alignment_data  %>% pivot_wider(values_from = c("plan_alloc_wt_tech_prod","scen_alloc_wt_tech_prod"), names_from = year) %>% 
    transmute(
      portfolio_name,
      asset_type,
      plan_sec_carsten,
      plan_build_out = plan_alloc_wt_tech_prod_2024 - plan_alloc_wt_tech_prod_2019,
      scen_build_out = scen_alloc_wt_tech_prod_2024 - scen_alloc_wt_tech_prod_2019,
      technology_climate_alignment_AT = plan_build_out / scen_build_out * 100
    ) %>% 
    left_join(
      asset_type_exposure, by = c("portfolio_name","asset_type")
    ) %>% 
    group_by(
      portfolio_name
    ) %>% 
    mutate(
      technology_climate_alignment = weighted.mean(technology_climate_alignment_AT, asset_type_exposure * plan_sec_carsten, na.rm = T)
    ) %>% ungroup()
  
  technology_climate_alignment_data <- technology_climate_alignment_data %>% 
    transmute(
      portfolio_name,
      technology_climate_alignment = round(
        case_when(
          technology_climate_alignment > 200 ~ 200.00,
          technology_climate_alignment < 0 ~ 0.00,
          T ~ technology_climate_alignment),2)
    ) %>% distinct() 
  
  technology_climate_alignment_data <- technology_climate_alignment_data %>%
    set_names("portfolio_name",paste0("climate_alignment_", str_remove(technology_selection,"cap")))
  
}


climate_alignment_renewables_data <- calculate_technology_climate_alignment(pacta_input_portfolio = input_pacta_portfolio_results, 
                                                                            start_year = START.YEAR, 
                                                                            technology_selection = "renewablescap", 
                                                                            scenario_selection = "B2DS")

climate_alignment_electric_data <- calculate_technology_climate_alignment(pacta_input_portfolio = input_pacta_portfolio_results, 
                                                                          start_year = START.YEAR, 
                                                                          technology_selection = "electric", 
                                                                          scenario_selection = "B2DS")


complete_fund_matrix <- complete_fund_matrix %>% left_join(climate_alignment_renewables_data, by = c("fund_isin" = "portfolio_name"))
complete_fund_matrix <- complete_fund_matrix %>% left_join(climate_alignment_electric_data, by = c("fund_isin" = "portfolio_name"))

complete_fund_matrix <- complete_fund_matrix %>% 
  mutate(
    climate_alignment_electric = if_else(is.na(climate_alignment_electric) & sector_exposure_automotive > 0, 0, climate_alignment_electric),
    climate_alignment_renewables = if_else(is.na(climate_alignment_renewables) & sector_exposure_power > 0, 0, climate_alignment_renewables)
  )



# ~ Regional exposure ~ 
#   TODO: load list of countries that are considered non-democratic and apply in grouping function
#   non democratic countries # social indicator # bool
#   TODO: include sovereign bonds in this analysis! 
dictatorship_list <- read.csv(paste0(DATA.PATH,"/04_Other/4_dictatorship_data/dictatorship data.csv"), strip.white = T, stringsAsFactors = F) %>% 
  select(
    cca2
  ) %>% left_join(
    regions, by = c("cca2" = "alpha-2")
  ) %>% pull(name)

geographical_exposure_dictatorships <- summarise_dictatorship_exposure(audit_file = pacta_audit,
                                                                       country_list = dictatorship_list,
                                                                       fund_size_data = complete_fund_matrix,
                                                                       fund_size_indicator = "fund_size_covered")

complete_fund_matrix <- complete_fund_matrix %>% left_join(geographical_exposure_dictatorships, by = c("fund_isin" = "portfolio_name")) %>% 
  mutate(geographical_exposure_dictatorships = if_else(is.na(geographical_exposure_dictatorships),0,geographical_exposure_dictatorships))



# TODO: Map output fund-isin vs country-code ISO2 with % exposure in the table
top5_countries <- group_ranker(input_audit = pacta_audit,
                               n_top = 5,
                               group_var = country_of_domicile,
                               group_name = "countries",
                               remove = "") 

complete_fund_matrix <- complete_fund_matrix %>% left_join(top5_countries, by = c("fund_isin" = "portfolio_name"))

calculate_coverage <- function(
  nominator_input_data, 
  denominator_input_data,
  output_value_name,
  coverage_indicator,
  nominator_value = "value_usd", 
  denominator_value = "fund_size", 
  include_na_in_coverage = FALSE, 
  group_vars = "fund_isin"
){
  
  input_data_indicator = nominator_input_data %>% 
    select(all_of(group_vars),
           all_of(coverage_indicator),
           all_of(nominator_value))
  
  if(include_na_in_coverage == FALSE){
    input_data_indicator <- input_data_indicator %>% filter(!is.na(coverage_indicator))
  }
  
  input_data_indicator <- input_data_indicator  %>% 
    rename("nominator" = {{nominator_value}})%>% 
    group_by(!!!syms(group_vars)) %>% 
    summarise(
      coverage_indicator_value = sum(nominator,na.rm = T)
    ) %>% ungroup()
  
  coverage_data_indicator <- input_data_indicator %>% 
    left_join(denominator_input_data %>% select(all_of(group_vars),all_of(denominator_value)) %>% rename("denominator" = {{denominator_value}}), by = group_vars) %>% 
    mutate(
      coverage = if_else(
        is.na(denominator) | denominator == 0, 
        as.numeric(NA), 
        coverage_indicator_value/denominator)
    ) %>% select(all_of(group_vars),coverage) %>% 
    rename({{output_value_name}} := coverage)
  
  
}

country_coverage_input_data <- pacta_audit %>% transmute(fund_isin = portfolio_name, country_of_domicile, value_usd, isin, asset_type, flag, holding_id) %>% distinct() %>%
  filter(!is.na(country_of_domicile) & !is.na(value_usd) & !country_of_domicile %in% c(remove))

coverage_top5_countries <- calculate_coverage(
  nominator_input_data = country_coverage_input_data,
  denominator_input_data = complete_fund_matrix,
  output_value_name = "country_exposure_coverage",
  coverage_indicator = "country_of_domicile",
  nominator_value = "value_usd",
  denominator_value = "fund_size",
  include_na_in_coverage = FALSE, 
  group_vars = "fund_isin")

complete_fund_matrix <- complete_fund_matrix %>% 
  left_join(
    coverage_top5_countries,
    by = "fund_isin"
    )

# ~ Top sector exposure ~
# TODO: compare to other sector exposure calculations and align!
# Exposure to Top 5 sectors # general information
top5_sectors <- group_ranker(
  input_audit = pacta_audit,
  n_top = 5,
  group_var = security_bics_subgroup,
  group_name = "sectors",
  remove = c("Sovereign", "Sovereign Agency", "Sovereigns")
)

complete_fund_matrix <- complete_fund_matrix %>% 
  left_join(
    top5_sectors, 
    by = c("fund_isin" = "portfolio_name")
  )

# ~ Top holdings exposure ~
# TODO: compare to direct output from MS!
# Exposure to Top 5 holdings # general information
top5_holdings <- group_ranker(
  input_audit = pacta_audit,
  n_top = 5,
  group_var = company_name,
  group_name = "holdings",
  remove = c("", " ")
)

complete_fund_matrix <- complete_fund_matrix %>% 
  left_join(
    top5_holdings, 
    by = c("fund_isin" = "portfolio_name")
  )

# ~ governance and 3rd party values ~
complete_fund_matrix <- complete_fund_matrix %>% 
  mutate(
    un_pri_signatory_flag = NA,
    uno_global_compact_perc = NA,
    msci_esg_rating = NA,
    fng_label = NA,
    controversies = NA,
    climate_lobbying_rating = NA,
    climate_lobbying_rating_coverage = NA,
    oecd_guidlines_perc = NA,
    technology_share_fuel_cell = NA
  )

# ~ apply traffic light data and calculate fonds traffic light values ~
apply_traffic_light_system <- function(input_data = complete_fund_matrix, traffic_light_data = traffic_light_values, indicator_list, group_vars = "fund_isin"){
  input_data_indicator <- input_data %>% 
    select(!!!syms(group_vars),!!!syms(indicator_list)) %>% 
    pivot_longer(-group_vars,values_to = "value", names_to = "indicator_name")
  
  traffic_light_data_indicator <- input_data_indicator %>% left_join(traffic_light_data, by = "indicator_name")
  
  traffic_light_data_indicator <- traffic_light_data_indicator %>% 
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

traffic_light_indicator_list <- c(unique(traffic_light_values$indicator_name))

traffic_light_data_indicators <- apply_traffic_light_system(
  input_data = complete_fund_matrix, 
  traffic_light_data = traffic_light_values,
  indicator_list = traffic_light_indicator_list,
  group_vars = "fund_isin"
)

complete_fund_matrix <- complete_fund_matrix %>% left_join(traffic_light_data_indicators, by = "fund_isin")

complete_fund_matrix %>% 
  check_for_duplicates(id = "fund_isin")

# TODO: add governance and 3rd party data points
sensitive_sectors <- sensitive_sectors %>% 
  filter(sector != "tobacco farming")

# ~ convert format of indicators to bool for the selected list of indicators ~
list_bool_indicators <- as.character(project_indicator_selection %>% filter(.data$data_type_in_output_file == "bool", .data$include_in_output_file) %>% 
                                       pull(.data$indicator))

fund_data_complete_formated <- convert_multiple_integer_to_bool(complete_fund_matrix,list_bool_indicators)

list_perc_indicators <- as.character(project_indicator_selection %>% filter(.data$data_type_in_output_file == "%", .data$include_in_output_file) %>% 
                                       pull(.data$indicator))

fund_data_complete_formated <- fund_data_complete_formated %>% 
  mutate_at(.vars = list_perc_indicators, .funs = convert_int_to_perc)

# ~ save entire fund matrix, including all available indicators ~
write.csv(fund_data_complete_formated, paste0(PROJ.LOCATION, "40_Results/", Project.Name, "Fund_Data_complete.csv"), row.names = F, na = "")
write.csv(fund_data_complete_formated %>% filter(fund_isin == "Meta Portfolio"), paste0(PROJ.LOCATION, "40_Results/", Project.Name, "Meta_Portfolio_results.csv"), row.names = F, na = "")


# subset and rename fund matrix
filter_and_rename_fund_matrix <- function(fund_matrix = fund_data_complete_formated, 
                                          formatting_file = project_indicator_selection){
  formatting_file <- formatting_file %>% filter(include_in_output_file) %>% distinct()
  
  fund_matrix <- fund_matrix %>% select(all_of(formatting_file$indicator)) %>% 
    set_names(formatting_file$name_in_output_file)
}

fund_data_formated_subset <- filter_and_rename_fund_matrix(fund_matrix = fund_data_complete_formated, 
                                                           formatting_file = project_indicator_selection) %>% 
  filter(fund_isin != "Meta Portfolio")

# filter fund database to funds with at least x% coverage (defined as coverage_threshold)
fund_list_sufficient_coverage <- complete_fund_matrix %>% filter(fund_size_covered > fund_size * coverage_threshold)
fund_data_with_sufficient_coverage <- fund_data_formated_subset %>% filter(fund_isin %in% fund_list_sufficient_coverage$fund_isin)

write_csv(fund_data_with_sufficient_coverage, paste0(PROJ.LOCATION, "40_Results/", Project.Name, "Fund_Data.csv"), na = "")

# ~ save entire fund matrix, including all available indicators ~
write_csv(fund_data_complete_formated %>% filter(fund_isin %in% fund_list_sufficient_coverage$fund_isin), paste0(PROJ.LOCATION, "40_Results/", Project.Name, "Fund_Data_all data points.csv"), na = "")









# Graphing
# Histogram












# TODO: get lists and load those for both UN PRI and UNO Global Compact
# TODO: write functions to apply the lists (using the grouping function by Vincent)
# Signator of UN PRI # ethic and governance indicator # bool
# UNO Global Compact # ethic and governance indicator

randomize_from_dim1 <- function(m, .f, ...) {
  .f(dim(m)[[1]], ...)
}

un_pri_signatory_flag <- asset_type_exposure_perc %>% 
  randomize_from_dim1(runif, 0, 1) %>% 
  round()

uno_global_compact_perc <- asset_type_exposure_perc %>% 
  randomize_from_dim1(rnorm, mean = 70, sd = 20)

oecd_guidlines_perc <- asset_type_exposure_perc %>% 
  randomize_from_dim1(rnorm, mean = 60, sd = 30)


test_df <- tibble(un_pri_signatory_flag, uno_global_compact_perc,oecd_guidlines_perc) %>% 
  mutate(
    uno_global_compact_perc = round(uno_global_compact_perc,2),
    oecd_guidlines_perc = round(oecd_guidlines_perc,2)
  ) %>% 
  mutate(
    uno_global_compact_perc = case_when(
      uno_global_compact_perc >= 100 ~ 100.00,
      uno_global_compact_perc <= 0 ~ 0,
      TRUE ~ uno_global_compact_perc
    ),
    oecd_guidlines_perc = case_when(
      oecd_guidlines_perc >= 100 ~ 100,
      oecd_guidlines_perc <= 0 ~ 0,
      TRUE ~ oecd_guidlines_perc
    )
    
  )

governance_indicators <- bind_cols(asset_type_exposure_perc %>% select(Portfolio.Name),test_df)


# - NKI list other indicators needed for final
# Trailing returns (3 yr) # financial indicator
# Sharpe ratio (3 yr) # financial indicator
# Volatility (Standard deviation 3yr) # financial indicator
#TODO: write small 1 to 1 "hand-over" and potentially renaming


# TODO: calculate from MS data or if done already extract from MS input file (ask Vincent about this)
# Coverage of holding data

# TODO: write name filter for most common words in a fund and check MS data field on this (Vincent might be able to help) discuss interface with Constanze
# Management strategy # general information # e.g. Best in class, passive, etc.

# Regional exposure 
#   per country for a map! # general information

# TODO: write 3rd Party interface function
# 3rd party values
#   FNG Siegel # ESG indicator
#   MSCI ESG # only if we get them - looks unlikely # ESG indicator



# TODO: see above, except green, brown & Temparature range indicators - not needed for NKI/Retail
# - LITA list
# Sector exposure
#   Top 5 sectors
# Regional exposure: 
#   Exposure top 2 countries
#   France 
#   Rest of Europe
# Holding Exposure 
#   top 10 holdings
# Exposure green, brown # climate indicator
# Temperature range indicator


# TODO: write workflow script after all other functions are ready
# Workflow
# 1) Load Indicator List 
# 2) Load Data Sets
# 3) Run <<retail_fund_results_gathering>> function
# 4) Add relevant 3rd party data if relevant
# 5) Rename columns to reflect parameter file choice
# 6) Save Fund result matrix


# Load Indicator List
load_indicator_selection <- function(){}

#' Retail fund results gathering and formatting
#'
#' @param fund_list 
#' @param output_indicators 
#'
#' @return
#' @export
#'
#' @examples
gather_retail_fund_results <- function(
  fund_list = nki_fund_universe, 
  output_indicator_list = nki_indicator_list, 
  output_indicator_names = nki_indicator_names){
  
  
}


# change type of indicator to bool for a list of indicators



# 3rd Party data interface function
# TODO: split between fund level and company level
add_3rd_party_datapoint <- function(
  fund_results, 
  third_party_data, 
  indicator_colname, 
  indicator_name_result_list, 
  mapping_id = fund_isin){
  
}










# ##################################################################### #
# # Script for LITA
# ##################################################################### #
# 
# 
# library(tidyverse)
# 
# #TO-DO 
# # clean ranking code 
# 
# ##################################################################### #
# #loading data 
# ##################################################################### #
# 
# #sector, revenue, and scenario data 
# sector_weightings <- read_csv(paste0(GIT.PATH, "Reference/ReferenceData/tech_sector_weighting.csv"))
# scenario_relationships <- read_csv(paste0(GIT.PATH, "Reference/ReferenceData/scenario_relationships.csv"))
# revenue_data <- read_rds(paste0(GIT.PATH, "Reference/ReferenceData/revenue_data.rda"))
# sensitive_sectors <- read_csv(paste0(GIT.PATH, "Reference/referencedata/sensentive_sectors.csv"))
# regions <- read_csv(paste0(GIT.PATH, "Reference/referencedata/regions.csv"))
# 
# 
# #financial data 
# fin_data <- read_csv(paste0(DATA.STORE.PATH, "financial_data_", FINANCIAL.TIMESTAMP, ".csv")) %>% 
#   mutate(bloomberg_id = as.character(bloomberg_id))
# 
# brown_technologies_list <- c("Oil", "Gas", "Coal", "CoalCap", "OilCap", "GasCap", "ICE")
# 
# #results 
# input_debt <- read_rds(paste0(PROJ.LOCATION, "40_Results/", Project.Name, "_Bonds-PortInput-Port.rda"))
# input_equity <- read_rds(paste0(PROJ.LOCATION, "40_Results/", Project.Name, "_Equity-PortInput-Port.rda"))
# 
# input_debt$Asset.Type <- "Bonds"
# input_equity$Asset.Type <- "Equity"
# 
# input_results <- bind_rows(input_debt, input_equity)
# 
# #audit 
# input_audit <- read_rds(paste0(PROJ.LOCATION, "30_Processed_Inputs/", Project.Name,"-AUDIT-Portfolio-Coverage.rda")) %>% 
#   mutate(bloomberg_id = as.character(bloomberg_id)) 
# 
# 
# ##################################################################### #
# #code to rank funds based on different groupings
# ##################################################################### #
# 
# source("fund_analysis_functions/ranking_function.R")
# 

# 
# 

# 
# rankings <- top2_countries %>% 
#   full_join(top5_sectors, by = c("Investor.Name", "Portfolio.Name")) %>% 
#   full_join(top10_holdings, by = c("Investor.Name", "Portfolio.Name"))
# 
# rm(top5_sectors, top2_countries, top10_holdings)
# 
# ##################################################################### #
# # code to calculate exposure to different countries and regions
# ##################################################################### #
# source("fund_analysis_functions/geography_exposure_function.R")
# 
# country_exposure <- geography_exposure(input = input_audit, 
#                                        geography = "country", 
#                                        filter = "France")
# 
# region_exposure <- geography_exposure(input = input_audit, 
#                                       geography = "region", 
#                                       filter = "Europe")
# 
# geography_exposure <- region_exposure %>% 
#   full_join(country_exposure, by = c("Investor.Name", "Portfolio.Name"))
# 
# 
# rm(country_exposure, region_exposure)
# 
# ##################################################################### #
# # code to calculate sensentive sector classifications 
# ##################################################################### #
# source("fund_analysis_functions/sector_exposure_function.R")
# 
# sector_exposure <- exposure_sectors(input = input_audit, 
#                                     type = "flag")
# 
# 
# ##################################################################### #
# # temperature range metric
# ##################################################################### #
# source("fund_analysis_functions/single_indicator_function.R")
# 
# #calculating "temperature" for each sector 
# temp <- single_indicator(
#   input_results = input_results,
#   upper_temp_threshold = 10,
#   lower_temp_threshold = 1.5,
#   start_year = 2019,
#   time_horizon = 5,
#   allocation = "PortfolioWeight")
# 
# #apply InfluenceMap rollup 
# temp_port <- influencemap_weighting_methodology(
#   input_results = temp,
#   input_audit = input_audit,
#   metric = temperature)
# 
# #finding exposure to climate relevant sectors 
# coverage <- mapped_sector_exposure(
#   input_audit = input_audit)
# 
# 
# #connecting temp. metric and coverage 
# temp_metric <- temp_port %>%
#   distinct(Investor.Name, Portfolio.Name, temperature) %>%
#   inner_join(coverage, 
#              by = c("Investor.Name", "Portfolio.Name"))
# 
# temp_metric <- range_finder(input_temp = temp_metric, 
#                             range = c(1.75, 2, 2.75, 3.5))
# 
# rm(temp_port, temp)
# 
# ##################################################################### #  
# # green brown split  
# ##################################################################### #  
# source("fund_analysis_functions/green_brown_exposure_function.R")
# 
# green_brown_exposure <- green_brown_exposure(
#   input_audit = input_audit)
# 
# ##################################################################### #
# # binding everything together 
# ##################################################################### #
# output <- rankings %>% 
#   full_join(geography_exposure, by = c("Investor.Name", "Portfolio.Name")) %>% 
#   full_join(sector_exposure, by = c("Investor.Name", "Portfolio.Name")) %>% 
#   full_join(temp_metric, by = c("Investor.Name", "Portfolio.Name")) %>% 
#   full_join(green_brown_exposure, by = c("Investor.Name", "Portfolio.Name"))