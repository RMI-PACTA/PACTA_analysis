calculate_ownership_weight <- function(portfolio) {
  portfolio <- portfolio %>%
    mutate(ownership_weight = number_of_shares / current_shares_outstanding_all_classes)

  return(portfolio)
}
