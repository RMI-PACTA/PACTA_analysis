check_valid_input_value <- function(portfolio_total) {

  # Currency negative or missing market value/number of shares
  portfolio_total <- portfolio_total %>%
    mutate(has_valid_input = case_when(
      is.na(market_value) & is.na(number_of_shares) ~ FALSE,
      market_value < 0 ~ FALSE,
      number_of_shares < 0 ~ FALSE,
      # !currency %in% currencies$currency ~ FALSE,
      TRUE ~ TRUE
    ))

  portfolio_total
}
