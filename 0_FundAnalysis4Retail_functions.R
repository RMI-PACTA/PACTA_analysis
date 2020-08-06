library(tidyverse)
library(janitor)

# hi Klaus
# hi Vincent

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

# load data sets if needed (check indicator list for this)
load_fund_results <- function(
  git_path,
  project_location
){
  # Other Data: sector, revenue, and scenario data
  # TODO: check if all is needed
  
  # prep influencemap sector and technology weights 
  sector_weightings <- read_csv(paste0(git_path, "Reference/ReferenceData/sector_weightings.csv")) #tech_sector_weighting.csv")) 
  
  sector_weightings <- sector_weightings %>% 
    clean_names(case = "snake") %>% 
    mutate_at(
      .vars = c("sector", "technology"),
      tolower
    )
  
  sector_weightings <<- sector_weightings
  
  #prep scenario relationships file 
  scenario_relationships <- read_csv(paste0(git_path, "Reference/ReferenceData/scenario_relationships.csv"))
  
  scenario_relationships <<- scenario_relationships %>% 
    clean_names(case = "snake") %>% 
    mutate_at(
      .vars = c("sector"),
      tolower
    )
  
  # prepare revenue data 
  revenue_data <- read_rds(path(git_path, "Reference/ReferenceData/revenue_data", ext = "rda"))
  
  revenue_data <- revenue_data %>% 
    clean_names(case = "snake") %>% 
    mutate_at(
      .vars = c("sector", "asset_type", "sub_sector"),
      tolower
    )
  
  revenue_data <- revenue_data %>% 
    mutate(tot_rev = if_else(tot_rev > 100, 100, tot_rev))
  
  revenue_data <<- revenue_data
  
  # prepare sensitive sectors 
  sensitive_sectors <- read_csv(paste0(git_path, "Reference/ReferenceData/sensentive_sectors.csv"))
  
  sensitive_sectors <- sensitive_sectors %>% 
    clean_names(case = "snake") %>% 
    mutate_at(
      .vars = c("asset_type", "sector"),
      .funs = tolower
    )
  
  sensitive_sectors <- sensitive_sectors %>% 
    mutate(
      tot_rev = if_else(tot_rev > 100, 100, tot_rev),
      id = as.character(id)
      )
  
  sensitive_sectors <- sensitive_sectors %>% 
    bind_rows(revenue_data) %>% 
    distinct_all()
  
  sensitive_sectors <<- sensitive_sectors 

  # prepare regions 
  regions <<- read_csv(paste0(GIT.PATH, "Reference/referencedata/regions.csv"))
  
  # indicator selection
  project_indicator_selection <<- read_csv(paste0(project_location, "10_Parameter_File/fund_results_indicator_selection.csv"))
  
  # raw MorningStar data
  general_fund_data <- read_csv(paste0(project_location, "20_Raw_Inputs/", Project.Name, "_fund_info.csv"))
  
  general_fund_data <- general_fund_data %>% 
    clean_names(case = "snake")
  
  project_general_fund_data <<- general_fund_data
  
  #prepare PACTA results
  pacta_portfolio_debt_results <- read_rds(paste0(project_location, "40_Results/Bonds_results_portfolio.rds"))
  pacta_portfolio_equity_results <- read_rds(paste0(project_location, "40_Results/Equity_results_portfolio.rds"))
  
  pacta_portfolio_debt_results$asset_type <- "Bonds"
  pacta_portfolio_equity_results$asset_type <- "Equity"
  
  pacta_portfolio_results <- pacta_portfolio_debt_results %>% 
    bind_rows(pacta_portfolio_equity_results)
  
  pacta_portfolio_results <- pacta_portfolio_results %>% 
    filter((ald_sector == "Power" & scenario_geography == "GlobalAggregate") | (ald_sector != "Power" & scenario_geography == "Global"))
    
  pacta_portfolio_results <- pacta_portfolio_results %>%   
    filter(investor_name != portfolio_name | portfolio_name == "Meta Portfolio")
  
  pacta_portfolio_results <- pacta_portfolio_results %>% 
    mutate_at(
      .vars = c("ald_sector", "technology", "asset_type"),
      .funs = tolower
    )

  input_pacta_portfolio_results <<- pacta_portfolio_results
  
  # prepare processed portfolio 
  pacta_audit_flag <- read_csv(paste0(project_location, "30_Processed_Inputs/", Project.Name, "_audit_file.csv")) %>% 
    distinct(portfolio_name, isin, company_name, asset_type, financial_sector, has_ald_in_fin_sector)
    
  pacta_audit <- read_csv(paste0(project_location, "30_Processed_Inputs/", Project.Name, "_total_portfolio.csv")) 
  
  pacta_audit <- pacta_audit %>% 
    mutate(bloomberg_id = as.character(bloomberg_id),
           value_usd = if_else(is.infinite(value_usd),0,value_usd)) %>% 
    filter(investor_name != portfolio_name | portfolio_name == "Meta Portfolio") %>% 
    left_join(pacta_audit_flag, by = intersect(colnames(pacta_audit),colnames(pacta_audit_flag)))

  pacta_audit <- pacta_audit %>% 
    mutate(across(matches(c("asset_type", "financial_sector", "security_mapped_sector")), tolower)) 
  
  Ports.Overview <<- read_csv(paste0(PROC.INPUT.PATH,Project.Name,"_overview_portfolio.csv"))

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




