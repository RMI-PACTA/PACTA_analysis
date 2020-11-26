default_dirpath <- "inst/extdata"

data_object_names <-
  c(
    "average_sector_intensity",
    "bics_bridge",
    "company_emissions",
    "consolidated_financial",
    "debt_financial",
    "exchange_rates",
    "fin_sector_overrides",
    "funds",
    "non_distinct_isins",
    "revenue",
    "sector_bridge",
    "security_financial"
  )

default_filenames_sans_ext <-
  c(
    average_sector_intensity = "average_sector_intensity",
    bics_bridge = "bics_bridge",
    company_emissions = "company_emissions",
    consolidated_financial = "consolidated_financial",
    debt_financial = "debt_financial",
    exchange_rates = "exchange_rates",
    fin_sector_overrides = "fin_sector_overrides",
    funds = "funds",
    non_distinct_isins = "non_distinct_isins",
    revenue = "revenue",
    sector_bridge = "sector_bridge",
    security_financial = "security_financial"
  )

common_filenames_sans_ext <-
  list(
    average_sector_intensity = c("average_sector_intensity", "average_sector_intensity_data"),
    bics_bridge = c("bics_bridge", "bics_bridge_data"),
    company_emissions = c("company_emissions", "company_emissions_data"),
    consolidated_financial = c("consolidated_financial", "consolidated_financial_data"),
    debt_financial = c("debt_financial", "debt_financial_data"),
    exchange_rates = c("exchange_rates", "exchange_rates_data", "currencies", "currencies_data", "currency_data"),
    fin_sector_overrides = c("fin_sector_overrides", "fin_sector_overrides_data"),
    funds = c("funds", "funds_data", "fund_data"),
    non_distinct_isins = c("non_distinct_isins", "non_distinct_isins_data"),
    revenue = c("revenue", "revenue_data"),
    sector_bridge = c("sector_bridge", "sector_bridge_data"),
    security_financial = c("security_financial", "security_financial_data", "fin_data")
  )


sector_list <- c("Automotive", "Power", "Oil&Gas", "Coal")
other_sector_list <- c("Shipping", "Steel", "Aviation", "Cement")


cb_groups <-
  c(
    "Convertible Bonds",
    "Corporate Bonds",
    "Corporate inflation linked Bonds",
    "Convertible Preferreds"
  )

sb_groups <-
  c(
    "Sovereign Debt",
    "Sovereign Agency Debt",
    "Government inflation linked Bonds",
    "Sovereign",
    "Sovereign Agency",
    "Sovereigns"
  )


get_cb_groups <-
  function() {
    cb_groups
  }


get_sb_groups <-
  function() {
    sb_groups
  }


in_groups <-
  function(x, group) {
    x %in% base::get(group)
  }


in_cb_groups <-
  function(x) {
    in_groups(x, "cb_groups")
  }


in_sb_groups <-
  function(x) {
    in_groups(x, "sb_groups")
  }
