summarize_fund_coverage  <- function(fund_portfolio) {

  fund_portfolio %>% select(investor_name,
                            portfolio_name,
                            isin,
                            value_usd,
                            company_name,
                            effective_coverage)
  return(fund_portfolio)
}
