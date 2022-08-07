check_financial_data <- function(portfolio_total) {
  portfolio_total <- portfolio_total %>%
    mutate(has_fin_data = case_when(
      (asset_type == "Equity" | asset_type == "Unclassifiable") & (is.na(company_id) | company_id == "") ~ FALSE,
      (asset_type == "" | asset_type == "Unclassifiable") ~ FALSE,
      is.na(asset_type) ~ FALSE,
      TRUE ~ TRUE
    ))

  portfolio_total
}
