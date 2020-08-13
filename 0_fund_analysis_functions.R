library(tidyverse)
library(janitor)
library(assertr)
library(fs)

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




