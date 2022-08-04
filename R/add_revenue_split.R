add_revenue_split <- function(has_revenue, portfolio, revenue_data) {
  if (has_revenue) {
    revenue_data_min <- revenue_data %>%
      filter(!is.na(company_id)) %>%
      select(-company_name, -equity_ticker, -corporate_bond_ticker, -bloomberg_id)

    initial_portfolio_value <- sum(portfolio$value_usd, na.rm = T)

    port_rev <- left_join(portfolio, revenue_data_min, by = "company_id", all.x = T)


    # Fill in gaps where possible
    port_rev <- port_rev %>%
      mutate(
        has_revenue_data = if_else(is.na(has_revenue_data), FALSE, has_revenue_data),
        tot_rev = if_else(is.na(tot_rev), 1, tot_rev),
        revenue_sector = if_else(is.na(revenue_sector), "Other", revenue_sector),
        value_usd = value_usd * tot_rev
      ) %>%
      rename(financial_sector = revenue_sector)

    if (sum(port_rev$value_usd, na.rm = T) != initial_portfolio_value) {
      stop("Revenue data causing duplications")
    }
  } else {
    port_rev <- portfolio %>%
      mutate(
        has_revenue_data = FALSE,
        financial_sector = security_mapped_sector
      )
  }

  return(port_rev)
}
