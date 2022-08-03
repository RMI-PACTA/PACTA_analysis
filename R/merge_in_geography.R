merge_in_geography <- function(portfolio, ald_raw) {

  # ald_raw <- ald_raw_eq
  company_all <- portfolio %>%
    distinct(!!!rlang::syms(grouping_variables), allocation, allocation_weight, id, financial_sector)


  company_all <- company_all %>% filter(financial_sector %in% sector_list)

  ### join with MASTER to get country production
  company_all_data <- left_join(company_all, ald_raw %>% distinct(
    id, country_of_domicile, ald_location, year,
    ald_sector, technology, ald_production, ald_production_unit
  ),
  by = c("id" = "id", "financial_sector" = "ald_sector")
  ) %>%
    mutate(ald_sector = financial_sector)

  ### complete rows of technology within a sector - we need to have a row for each tech to get a real tech share
  # dont' calculate tech share
  # specific_tech_list <- unique(company_all_data$technology)
  # specific_sector_list <- unique(company_all_data$ald_sector)
  #
  # company_all_data <- company_all_data %>% ungroup() %>%
  #   group_by(investor_name, portfolio_name, allocation, id, financial_sector, allocation, allocation_weight, ald_location,
  #            year, ald_sector) %>%
  #   complete(ald_sector = specific_sector_list,
  #           technology = specific_tech_list,
  #            fill = list(ald_production = 0))
  #
  # company_all_data <- removeInvalidSectorTechCombos(company_all_data)

  company_all_data$plan_alloc_wt_tech_prod <- company_all_data$ald_production * company_all_data$allocation_weight


  return(company_all_data)
}
