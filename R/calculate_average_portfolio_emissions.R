calculate_average_portfolio_emissions <- function(portfolio_total,
                                                  comp_fin_data,
                                                  average_sector_intensity) {

  min_portfolio <- portfolio_total %>%
    select(
      investor_name,
      portfolio_name,
      financial_sector,
      security_mapped_sector,
      company_id,
      value_usd,
      asset_type
    ) %>%
    filter(asset_type %in% c("Equity", "Bonds")) %>%
    left_join(
      select(comp_fin_data, company_id, bics_sector),
      by = "company_id"
    ) %>%
    group_by(
      investor_name,
      portfolio_name,
      asset_type,
      financial_sector,
      bics_sector
    ) %>%
    summarise(value_usd = sum(value_usd, na.rm = T),  .groups = "drop") %>%
    left_join(
      select(
        average_sector_intensity,
        bics_sector,
        asset_type,
        median_intensity,
        mean_intensity
      ),
      by = c("bics_sector", "asset_type")
    )

  # Create averages where bics sectors were missing
  average_other_sectors <- min_portfolio %>%
    group_by(asset_type, financial_sector) %>%
    summarise(
      mean_intensity = mean(mean_intensity, na.rm = T),
      .groups = "drop"
    )

  min_portfolio <- min_portfolio %>%
    left_join(
      average_other_sectors,
      by = c("asset_type", "financial_sector")
    ) %>%
    mutate(
      mean_intensity = ifelse(
        is.na(mean_intensity.x),
        mean_intensity.y,
        mean_intensity.x
      )
    ) %>%
    select(-mean_intensity.x, -mean_intensity.y)  %>%
    mutate(weighted_sector_emissions = value_usd * mean_intensity)

  min_portfolio <- min_portfolio %>%
    mutate(
      sector = ifelse(
        financial_sector != "Other",
        financial_sector,
        bics_sector
      ),
      sector = ifelse(
        is.na(sector),
        "Other",
        sector
      )
    ) %>%
    group_by(investor_name, portfolio_name, asset_type,sector) %>%
    summarise(
      weighted_sector_emissions = sum(weighted_sector_emissions, na.rm = T),
      .groups = "drop"
    )

  return(min_portfolio)
}
