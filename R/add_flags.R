add_flags <- function(portfolio) {
  portfolio <- portfolio %>%
    mutate(flag = case_when(
      !has_currency ~ "Missing currency information",
      !has_valid_input ~ "Negative or missing input value",
      !has_valid_isin ~ "Invalid or missing ISIN",
      !has_bbg_data ~ "Holding not in Bloomberg database",

      TRUE ~ "Included in analysis"
    ))

  portfolio
}
