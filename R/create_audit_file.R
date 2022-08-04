create_audit_file <- function(portfolio_total) {

  # portfolio_total <- left_join(portfolio_total, comp_fin_data %>% select(company_id, sectors_with_assets, bics_sector), by = "company_id")
  #
  # portfolio_total <- portfolio_total %>% rowwise() %>%
  #   mutate(has_assets = ifelse(is.na(sectors_with_assets), TRUE, FALSE),
  #          has_assets_in_fin_sector = grepl(pattern = financial_sector, x = sectors_with_assets)
  #   )

  audit_file <- portfolio_total %>%
    select(
      all_of(grouping_variables), holding_id, isin, value_usd, company_name, asset_type, has_revenue_data, valid_input,
      direct_holding, financial_sector, bics_sector, sectors_with_assets, has_ald_in_fin_sector, flag
    )

  if (has_revenue == FALSE) {
    audit_file <- audit_file %>% select(-has_revenue_data)
  }

  return(audit_file)
}
