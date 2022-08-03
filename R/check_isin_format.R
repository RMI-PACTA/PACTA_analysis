check_isin_format <- function(portfolio_total) {
  portfolio_total <- portfolio_total %>%
    mutate(has_valid_isin = case_when(
      nchar(isin) != 12 ~ FALSE,
      isin == "" ~ FALSE,
      is.na(isin) ~ FALSE,
      grepl("[^[:alnum:]]", isin) ~ FALSE,
      TRUE ~ TRUE
    ))

  portfolio_total
}
