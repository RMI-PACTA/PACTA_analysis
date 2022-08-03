check_bloomberg_data <- function(portfolio_total) {
  portfolio_total <- portfolio_total %>%
    mutate(has_bbg_data = case_when(
      (asset_type == "Equity" | asset_type == "Unclassifiable") & (is.na(company_id) | company_id == "") ~ FALSE,
      (asset_type == "" | asset_type == "Unclassifiable") ~ FALSE,
      is.na(asset_type) ~ FALSE,
      TRUE ~ TRUE
    ))

  portfolio_total
}
