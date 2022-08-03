add_fin_data <- function(portfolio, fin_data) {
  left_join(portfolio, fin_data, by = "isin")
}
