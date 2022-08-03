clean_unmatched_holdings <- function(portfolio) {
  portfolio %>%
    mutate(asset_type = ifelse(is.na(security_mapped_sector), "Unclassifiable", asset_type)) %>%
    mutate(security_mapped_sector = ifelse(is.na(security_mapped_sector), "Unclassifiable", security_mapped_sector))
}
