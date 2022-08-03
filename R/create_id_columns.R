create_id_columns <- function(portfolio, portfolio_type) {
  if (portfolio_type == "Equity") {
    portfolio <- portfolio %>%
      rename(id = bloomberg_id) %>%
      mutate(
        id_name = "bloomberg_id",
        id = as.character(id)
      )
  }
  if (portfolio_type == "Bonds") {
    portfolio <- portfolio %>%
      rename(id = corporate_bond_ticker) %>%
      mutate(
        id_name = "corporate_bond_ticker",
        id = as.character(id)
      )
  }

  return(portfolio)
}
