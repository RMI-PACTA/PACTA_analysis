export_audit_invalid_data <- function(portfolio_total_, export_path_full) {
  portfolio_total_ <- portfolio_total_ %>% subset(flag %in% c("Missing currency information", "Negative or missing input value", "Invalid or missing ISIN"))
  portfolio_total_ <- filter(portfolio_total_, direct_holding == TRUE)
  portfolio_total_ <- portfolio_total_ %>% select("isin", "market_value", "currency", "flag")
  portfolio_total_ <- portfolio_total_[order(-portfolio_total_$market_value), ]

  colnames(portfolio_total_) <- c("isin", "marketValues", "currency", "flag")
  invalidsecurties <- toJSON(portfolio_total_, dataframe = c("columns"))

  write(invalidsecurties, file = paste0(export_path_full, ".json"))
  write_csv_file(portfolio_total_, file = paste0(export_path_full, ".csv"))
}
