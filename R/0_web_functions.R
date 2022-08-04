# 0_web_functions

identify_portfolios <- function(portfolio_total) {
  port_names <- portfolio_total %>%
    select(investor_name, portfolio_name) %>%
    distinct()

  port_names <- port_names %>%
    mutate(
      file_name = gsub(" ", "", portfolio_name),
      loc_name = paste0(portfolio_name_ref_all, "-", file_name)
    )


  if (length(port_names %>% select(investor_name) %>% distinct()) > 1) {
    write_log(
      msg = "There is more than one investor name within a portfolio. Please correct the input data and retry.",
      file_path = log_path
    )
    stop("More than one investor in portfolio")
  }

  return(port_names)
}

website_text <- function(audit_file, proc_input_path) {
  PortValues <- audit_file %>%
    ungroup() %>%
    filter(valid_input == TRUE) %>%
    summarize(PortTot = sum(value_usd))

  Asset.Values <- audit_file %>%
    ungroup() %>%
    group_by(asset_type) %>%
    filter(valid_input == TRUE) %>%
    summarise(Tot = sum(value_usd), .groups = "drop_last")

  asset_types <- c("Equity", "Bonds")

  if (length(setdiff(asset_types, Asset.Values$asset_type)) > 0) {
    newrow <- data.frame(asset_type = setdiff(asset_types, Asset.Values$asset_type), Tot = 0)
    Asset.Values <- rbind(Asset.Values, newrow)
  }

  Bonds.Value <- prettyNum(round(Asset.Values$Tot[Asset.Values$asset_type == "Bonds"], 0), big.mark = ",", scientific = FALSE)

  Equity.Value <- prettyNum(round(Asset.Values$Tot[Asset.Values$asset_type == "Equity"], 0), big.mark = ",", scientific = FALSE)

  text <- paste0("The portfolio you have uploaded has $", prettyNum(round(PortValues[[1]], 0), big.mark = ",", scientific = FALSE), " USD in holdings.
                 Of which $", Bonds.Value, " USD is in bonds,
                and $", Equity.Value, " USD is in equity.

                The remainder of the holdings are in asset classes outside the scope of this analysis.
                For more information as to how each holding is classified, review the chart and audit file below.")

  write(text, file = file.path(proc_input_path, "Websitetext.txt"))
}

save_cleaned_files <- function(save_loc,
                               currencies,
                               fund_data,
                               fin_data,
                               comp_fin_data,
                               debt_fin_data,
                               average_sector_intensity,
                               company_emissions,
                               total_fund_list = NULL) {
  if (!dir.exists(save_loc)) {
    dir.create(save_loc)
  }

  fst::write_fst(as.data.frame(currencies), file.path(save_loc, "currencies.fst"))
  fst::write_fst(fund_data, file.path(save_loc, "fund_data.fst"))
  fst::write_fst(fin_data, file.path(save_loc, "fin_data.fst"))
  fst::write_fst(comp_fin_data, file.path(save_loc, "comp_fin_data.fst"))
  fst::write_fst(debt_fin_data, file.path(save_loc, "debt_fin_data.fst"))
  fst::write_fst(average_sector_intensity, file.path(save_loc, "average_sector_intensity.fst"))
  fst::write_fst(company_emissions, file.path(save_loc, "company_emissions.fst"))
  if (!is.null(total_fund_list)) {
    fst::write_fst(total_fund_list, file.path(save_loc, "total_fund_list.fst"))
  }

  if (check_file_size(save_loc)) warning("File size exceeds what can be pushed to GitHub. Check before Committing")
}

check_file_size <- function(folder_to_check) {
  files_to_check <- list.files(folder_to_check, full.names = T)
  any(file.size(files_to_check) > 100e6)
}

empty_portfolio_results <- function(){
  tibble(
    "investor_name" = NA_character_, "portfolio_name" = NA_character_,
    "scenario" = NA_character_, "allocation" = NA_character_,
    "equity_market" = NA_character_, "scenario_geography" = NA_character_,
    "year" = NA_integer_, "ald_sector" = NA_character_, "technology" = NA_character_,
    "plan_tech_prod" = NA_integer_, "plan_alloc_wt_tech_prod" = NA_integer_,
    "plan_carsten" = NA_integer_, "plan_emission_factor" = NA_integer_,
    "scen_tech_prod" = NA_integer_, "scen_alloc_wt_tech_prod" = NA_integer_,
    "scen_carsten" = NA_integer_, "scen_emission_factor" = NA_integer_,
    "plan_sec_prod" = NA_integer_, "plan_alloc_wt_sec_prod" = NA_integer_,
    "plan_sec_carsten" = NA_integer_, "plan_sec_emissions_factor" = NA_integer_,
    "scen_sec_prod" = NA_integer_, "scen_alloc_wt_sec_prod" = NA_integer_,
    "scen_sec_carsten" = NA_integer_, "scen_sec_emissions_factor" = NA_integer_,
    "plan_tech_share" = NA_integer_, "scen_tech_share" = NA_integer_,
    "trajectory_deviation" = NA_integer_, "trajectory_alignment" = NA_integer_
  )
}

empty_company_results <- function(){
  tibble(
    "investor_name" = NA_character_, "portfolio_name" = NA_character_,
    "scenario" = NA_character_, "allocation" = NA_character_,
    "id" = NA_character_, "company_name" = NA_character_,
    "financial_sector" = NA_character_, "port_weight" = NA_integer_,
    "allocation_weight" = NA_integer_, "plan_br_dist_alloc_wt" = NA_integer_,
    "scen_br_dist_alloc_wt" = NA_integer_,
    "equity_market" = NA_character_, "scenario_geography" = NA_character_,
    "year" = NA_integer_, "ald_sector" = NA_character_, "technology" = NA_character_,
    "plan_tech_prod" = NA_integer_, "plan_alloc_wt_tech_prod" = NA_integer_,
    "plan_carsten" = NA_integer_, "plan_emission_factor" = NA_integer_,
    "scen_tech_prod" = NA_integer_, "scen_alloc_wt_tech_prod" = NA_integer_,
    "scen_carsten" = NA_integer_, "scen_emission_factor" = NA_integer_,
    "plan_sec_prod" = NA_integer_, "plan_alloc_wt_sec_prod" = NA_integer_,
    "plan_sec_carsten" = NA_integer_, "plan_sec_emissions_factor" = NA_integer_,
    "scen_sec_prod" = NA_integer_, "scen_alloc_wt_sec_prod" = NA_integer_,
    "scen_sec_carsten" = NA_integer_, "scen_sec_emissions_factor" = NA_integer_,
    "plan_tech_share" = NA_integer_, "scen_tech_share" = NA_integer_,
    "trajectory_deviation" = NA_integer_, "trajectory_alignment" = NA_integer_
  )
}

empty_emissions_results <- function(){
  tibble("investor_name" = NA_character_, "portfolio_name" = NA_character_,
         "asset_type" = NA_character_, "sector" = NA_character_,
         "weighted_sector_emissions" = NA_real_)
}

empty_audit_file <- function(){
  tibble("investor_name" = NA_character_, "portfolio_name" = NA_character_,
         "asset_type" = NA_character_, "valid_input" = NA, "isin" = NA_character_,
         "direct_holding" = NA, "value_usd" = NA_real_)
}

empty_map_results <- function(){
  tibble(
    "investor_name" = NA_character_, "portfolio_name" = NA_character_,
    "ald_location" = NA_character_, "year" = NA_integer_,
    "ald_sector" = NA_character_, "technology" = NA_character_,
    "financial_sector" = NA_character_, "allocation" = NA_character_,
    "allocation_weight" = NA_integer_, "ald_production_unit" = NA_character_,
    "plan_alloc_wt_tech_prod" = NA_integer_, "plan_alloc_wt_sec_prod" = NA_integer_,
    "equity_market" = NA_character_, "scenario" = NA_character_,
    "scenario_geography" = NA_character_
  )
}

empty_st_results <- function(){
  tibble(
    "investor_name" = NA_character_, "portfolio_name" = NA_character_,
    "ald_sector" = NA_character_, "technology" = NA_character_,
    "scenario_geography" = NA_character_, "VaR_technology" = NA_real_,
    "asset_portfolio_value" = NA_real_, "VaR_Sector" = NA_real_,
    "scenario_name" = NA_character_, "technology_exposure" = NA_real_,
    "ector_exposure" = NA_real_, "sector_loss" = NA_real_,
    "climate_relevant_var" = NA_real_, "portfolio_aum" = NA_real_,
    "portfolio_loss_percentage" = NA_real_, "year_of_shock" = NA_integer_,
    "duration_of_shock" = NA_integer_, "production_shock_percentage" = NA_real_
  )
}

empty_ipr_st_results <- function(){
  tibble(
    "investor_name" = NA_character_, "portfolio_name" = NA_character_,
    "sector" = NA_character_, "subsector" = NA_character_,
    "exposure" = NA_real_, "description" = NA_character_,
    "scenario" = NA_character_, "shock" = NA_real_,
    "loss" = NA_real_
  )
}

empty_portfolio_overview <- function(){
  tibble(
    "investor_name" = NA_character_, "portfolio_name" = NA_character_,
    "asset_type" = NA_character_, "financial_sector" = NA_character_,
    "valid_input" = NA, "valid_value_usd" = NA_real_,
    "asset_value_usd" = NA_real_, "portfolio_value_usd" = NA_real_
  )
}

