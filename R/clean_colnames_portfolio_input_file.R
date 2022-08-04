clean_colnames_portfolio_input_file <- function(portfolio) {
  if (is.data.frame(portfolio)) {
    # Removes additional columns added by Excel on saving
    portfolio <- portfolio[, !grepl("X", colnames(portfolio))]
  } else {
    stop("No portfolio Data readable")
  }

  portfolio <- janitor::clean_names(portfolio)

  if ("numberof_shares" %in% colnames(portfolio)) {
    portfolio <- portfolio %>% rename(number_of_shares = numberof_shares)
  }

  portfolio
}
