overall_validity_flag <- function(portfolio_total) {
  portfolio_total <- portfolio_total %>%
    mutate(valid_input = case_when(
      !has_currency ~ FALSE,
      !has_fin_data ~ FALSE,
      !has_valid_input ~ FALSE,
      !has_valid_isin ~ FALSE,
      TRUE ~ TRUE
    ))

  portfolio_total
}
