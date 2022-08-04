list_unknown_funds_in_funds <- function(portfolio_total) {

  fund_portfolio_total <- portfolio_total %>% filter(!is.na(fund_isin))

  table_of_funds_in_funds_not_mapped <- fund_portfolio_total %>%
    filter(nchar(isin)==12 & asset_type == "Funds") %>%
    mutate(direct_holding="FALSE") %>%
    select(investor_name,
           portfolio_name,
           isin,
           value_usd,
           company_name,
           fund_isin) %>% rename(parent_fund_isin = fund_isin)

  return(table_of_funds_in_funds_not_mapped)
}
