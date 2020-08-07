library(tidyverse)
library(janitor)
library(assertr)
library(fs)

# hi Klaus
# hi Vincen

summarise_pacta_sector_exposure <- function(data, ...) {
  # clean data 
  data <- data %>%
    assertr::verify(has_all_names(security_mapped_sector, financial_sector)) %>% 
    mutate(security_mapped_sector = tolower(security_mapped_sector))
  # create nice pacta sectors
  data <- data %>% 
    mutate(
      pacta_sector = case_when(
        security_mapped_sector %in% c("other", "unclassifiable") ~ "other", 
        security_mapped_sector == "coal" ~ "coal_mining", 
        TRUE ~ security_mapped_sector
      )
    )
  # summarise and then reclassify names 
  data <- data %>% 
    group_by(
      ...,
      pacta_sector, 
      asset_type
    ) %>%
    summarise(sector_exposure = sum(value_usd, na.rm = TRUE)) %>% 
    ungroup()
  # reclassify things 
  data <- data %>% 
    classify_asset_types() %>% 
    group_by(
      ...,
      pacta_sector, 
      asset_type
    ) %>%
    summarise(sector_exposure = sum(sector_exposure, na.rm = TRUE)) %>% 
    ungroup()
  # calculate share 
  data %>% 
    group_by(...)
  mutate(sector_exposure = sector_exposure / sum(sector_exposure, na.rm = TRUE)) 
}

classify_asset_types <- function(.data) {
  .data %>% 
    assertr::verify(has_all_names("security_bics_subgroup", "security_type", "flag")) %>% 
    mutate(
      asset_type = case_when(
        security_bics_subgroup %in% c("Sovereign","Sovereign Agency", "Sovereigns") ~ "sovereign",
        security_type %in% c("Sovereign Debt","Sovereign Agency Debt", "Government inflation linked Bonds") ~ "sovereign",
        flag == "Holding not in Bloomberg database" ~ "no_data_available",
        TRUE ~ asset_type
      )
    )
}

summarise_asset_type_exposure <- function(
  .data, 
  ...,
  value_usd = "value_usd"
) {
  .data <- .data %>% 
    assertr::verify(has_all_names(value_usd)) %>% 
    classify_asset_types()
  # summarize total asset type value 
  .data %>% 
    group_by(
      ..., 
      asset_type
    ) %>% 
    summarize(asset_type_value = sum(.data[[value_usd]], na.rm = TRUE)) %>% 
    group_by(...) %>% 
    mutate(asset_type_exposure = asset_type_value / sum(asset_type_value, na.rm = TRUE))
}

summarise_portfolio_value <- function(
  .data, 
  ...,
  value_usd = "value_usd"
) {
  .data %>% 
    group_by(...) %>% 
    summarise(fund_size = sum(.data[[value_usd]], na.rm = TRUE)) %>% 
    ungroup()
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

load_pacta_portfolio <- function(project_location) {
  # prepare processed portfolio 
  audit_flag <- read_rds(path(project_location, "30_Processed_Inputs", paste(project_name, "audit_file", sep = "_"), ext = "rda")) %>% 
    distinct(
      portfolio_name, 
      isin, 
      company_name, 
      asset_type, 
      financial_sector, 
      has_ald_in_fin_sector
    )
  
  portfolio <- read_rds(path(project_location, "30_Processed_Inputs", paste(project_name, "total_portfolio", sep = "_"), ext = "rda")) 
  
  portfolio <- portfolio %>% 
    mutate(
      bloomberg_id = as.character(bloomberg_id),
      value_usd = if_else(is.infinite(value_usd),0,value_usd)
    ) %>% 
    filter(investor_name != portfolio_name | portfolio_name == "Meta Portfolio") %>% 
    left_join(
      audit_flag, 
      by = intersect(colnames(portfolio),colnames(audit_flag))
      )
  
  portfolio %>% 
    mutate(across(matches(c("asset_type", "financial_sector", "security_mapped_sector")), tolower)) 
}

#prepare PACTA results
load_pacta_results <- function(project_location = project_location) {
  
  portfolio_debt_results <- read_rds(path(project_location, "40_Results", "Bonds_results_portfolio", ext = "rda"))
  portfolio_equity_results <- read_rds(path(project_location, "40_Results", "Equity_results_portfolio", ext = "rda"))
  
  portfolio_debt_results$asset_type <- "Bonds"
  portfolio_equity_results$asset_type <- "Equity"
  
  portfolio_results <- portfolio_debt_results %>% 
    bind_rows(portfolio_equity_results)
  
  portfolio_results <- portfolio_results %>% 
    filter((ald_sector == "Power" & scenario_geography == "GlobalAggregate") | (ald_sector != "Power" & scenario_geography == "Global"))
  
  portfolio_results <- portfolio_results %>%   
    filter(investor_name != portfolio_name | portfolio_name == "Meta Portfolio")
  
  portfolio_results %>% 
    mutate(across(matches(c("ald_sector", "technology", "asset_type")), tolower)) 
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



# Single Indicator function - should be loaded from r2dii.analysis instead!
influencemap_weighting_methodology <- function(
  pacta_portfolio_results,
  processed_portfolio,
  metric_col = "trajectory_alignment"
) {
  # load sector technology weights 
  sector_weightings <<- read_csv(paste0(GIT.PATH, "Reference/ReferenceData/sector_weightings.csv")) #tech_sector_weighting.csv"))
  
  sector_weightings <- sector_weightings %>% 
    mutate(across(everything(), tolower))
  
  results_file <- pacta_portfolio_results
  
  # first join technology and sector weights to pacta results file 
  results <- results_file %>%
    inner_join(
      sector_weightings, 
      by = c(
        "ald_sector" = "sector", 
        "technology" = "technology"
      )
    )
  
  # prepare audit file to show the relative portfolio weight in each sector 
  audit_file <- processed_portfolio %>%
    filter(security_mapped_sector == financial_sector) %>% 
    rename(ald_sector = security_mapped_sector)
  
  # calculate by asset_type the value_usd in each sector 
  audit_exposure <- audit_file %>% 
    group_by(
      investor_name, 
      portfolio_name, 
      ald_sector, 
      asset_type
    ) %>%
    summarise(value_usd_sector = sum(value_usd, na.rm = TRUE))
  
  # caculate for each port the value_usd in each asset_type 
  audit_exposure <- audit_exposure %>%
    group_by(
      investor_name, 
      portfolio_name,
      asset_type
    ) %>%
    mutate(value_usd_asset_type = sum(value_usd_sector, na.rm = TRUE))
  
  # join audit exposure to result file 
  results_audit <- audit_exposure %>%
    mutate(ald_sector = tolower(ald_sector)) %>% 
    filter(ald_sector != "other" & !is.na(ald_sector)) %>%
    inner_join(
      results, 
      by = c(
        "ald_sector", 
        "investor_name", 
        "portfolio_name", 
        "asset_type"
      )
    )
  
  # first reweight alignment to the sector level based on the technology importance 
  results_technology <- results_audit %>%
    filter(!is.na(technology_weight)) %>%
    group_by(
      investor_name, 
      portfolio_name, 
      asset_type, 
      ald_sector
    ) %>%
    mutate(
      technology_production_weight = as.numeric(technology_weight) * as.numeric(plan_alloc_wt_tech_prod),
      metric_sector = stats::weighted.mean(.data[[metric_col]], technology_weight, na.rm = TRUE)
    )
  
  # if no technology breakdown then just pass sectors alignment 
  results_sector <- results_audit %>%
    filter(is.na(technology_weight)) %>%
    mutate(metric_sector = .data[[metric_col]])
  
  # bind both calculations together
  results_sector <- bind_rows(
    results_technology, 
    results_sector
  )
  
  # then weight according to the port value in the sector and the sector importance 
  results_asset_type <- results_sector %>%
    group_by(
      investor_name, 
      portfolio_name, 
      asset_type
    ) %>%
    mutate(
      sector_value_usd_weight = as.numeric(value_usd_sector) * as.numeric(sector_weight),
      metric_asset_type = stats::weighted.mean(metric_sector, sector_value_usd_weight, na.rm = TRUE)
    )
  
  results_portfolio <- results_asset_type %>%
    group_by(
      investor_name, 
      portfolio_name
    ) %>%
    mutate(metric_port = stats::weighted.mean(metric_asset_type, value_usd_asset_type, na.rm = TRUE))
  
  results_portfolio %>% 
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




