# column specifications for datasets -------------------------------------------

colspec_average_sector_intensity_data <-
  function() {
    c(
      bics_sector = "character",
      mean_intensity = "numeric",
      median_intensity = "numeric",
      sd_intensity = "numeric",
      asset_type = "character",
      unit = "character"
    )
  }


colspec_bics_bridge_data <-
  function() {
    c(
      bics_sector = "character",
      bics_subgroup = "character"
    )
  }


colspec_company_emissions_data <-
  function() {
    c(
      company_id = "numeric",
      company_name = "character",
      ald_sector = "character",
      bics_sector = "character",
      bics_subgroup = "character",
      mapped_sector = "character",
      unit = "character",
      emissions_datastore = "numeric",
      emissions_trucost = "numeric",
      emissions = "numeric",
      source = "character"
    )
  }


colspec_consolidated_financial_data <-
  function() {
    c(
      company_id = "numeric",
      bloomberg_id = "numeric",
      company_name = "character",
      country_of_domicile = "character",
      corporate_bond_ticker = "character",
      equity_ticker = "character",
      mapped_sector = "character",
      bics_sector = "character",
      bics_subgroup = "character",
      icb_subgroup = "character",
      bclass4 = "character",
      has_asset_level_data = "logical",
      has_assets_in_matched_sector = "logical",
      sectors_with_assets = "character",
      market_cap = "numeric",
      free_float_shares = "numeric",
      current_shares_outstanding = "numeric",
      current_shares_outstanding_all_classes = "numeric",
      financial_timestamp = "character"
    )
  }


colspec_currency_data <-
  function() {
    c(
      Currency = "character",
      Currency_abbr = "character"
    )
  }


colspec_debt_financial_data <-
  function() {
    c(
      corporate_bond_ticker = "character",
      has_asset_level_data = "logical",
      sectors_with_assets = "character",
      bics_sector = "character"
    )
  }


colspec_fin_sector_overrides_data <-
  function() {
    c(
      company_name = "character",
      corporate_bond_ticker = "character",
      bloomberg_id = "numeric",
      fin_sector_override = "character"
    )
  }


colspec_fund_data <-
  function() {
    c(
      fund_isin = "character",
      holding_isin = "character",
      isin_weight = "numeric",
      fund_type = "character"
    )
  }


colspec_non_distinct_isins_data <-
  function() {
    c(
      isin = "character"
    )
  }


colspec_revenue_data <-
  function() {
    c(
      company_id = "numeric",
      company_name = "character",
      bloomberg_id = "numeric",
      equity_ticker = "character",
      corporate_bond_ticker = "character",
      has_revenue_data = "logical",
      sector = "character",
      tot_rev = "numeric"
    )
  }


colspec_sector_bridge <-
  function() {
    c(
      industry_classification = "character",
      source = "character",
      sector = "character",
      sector_ipr = "character",
      subsector_ipr = "character",
      sector_dnb = "character",
      sector_boe = "character",
      subsector_boe = "character"
    )
  }


colspec_security_financial_data <-
  function() {
    c(
      company_id = "numeric",
      bloomberg_id = "numeric",
      company_name = "character",
      country_of_domicile = "character",
      ticker = "character",
      corporate_bond_ticker = "character",
      isin = "character",
      asset_type = "character",
      security_icb_subsector = "character",
      security_bics_subgroup = "character",
      security_bclass4 = "character",
      security_mapped_sector = "character",
      security_type = "character",
      issue_date = "Date",
      unit_share_price = "numeric",
      market_value = "numeric",
      current_shares_outstanding = "numeric",
      current_shares_outstanding_all_classes = "numeric",
      financial_timestamp = "character"
    )
  }


colspec_by_name <-
  function(name) {
    do.call(paste0("colspec_", name), args = list())
  }
