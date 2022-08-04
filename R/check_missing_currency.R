check_missing_currency <- function(portfolio_total) {

  # Currency blank or not in our currency data frame
  portfolio_total <- portfolio_total %>%
    mutate(has_currency = case_when(
      is.na(currency) ~ FALSE,
      currency == "" ~ FALSE,
      !currency %in% currencies$currency ~ FALSE,
      TRUE ~ TRUE
    ))

  portfolio_total
}
