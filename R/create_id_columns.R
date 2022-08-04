create_id_columns <- function(portfolio, portfolio_type) {
  if (portfolio_type == "Equity") {
    portfolio <- portfolio %>%
      rename(id = fsym_id) %>%
      mutate(
        id_name = "fsym_id",
        id = as.character(id)
      )
  }
  if (portfolio_type == "Bonds") {
    portfolio <- portfolio %>%
      rename(id = fsym_id) %>%
      mutate(
        id_name = "fsym_id",
        id = as.character(id)
      )
  }

  return(portfolio)
}
