add_holding_id <- function(portfolio) {
  if (length(setdiff("holding_id", names(portfolio))) != 0) {
    portfolio$holding_id <- row.names(portfolio)
  }

  portfolio
}
