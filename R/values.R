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
    consolidated_financial = "consolidated_financial_data",
    debt_financial = "debt_financial_data",
    exchange_rates = "currencies",
    fin_sector_overrides = "fin_sector_overrides",
    non_distinct_isins = "non_distinct_isins",
    revenue = "revenue_data",
    sector_bridge = "sector_bridge",
    security_financial = "security_financial_data"
  )

common_filenames_sans_ext <-
  list(
    average_sector_intensity = "average_sector_intensity",
    bics_bridge = "bics_bridge",
    company_emissions = "company_emissions",
    consolidated_financial = c("consolidated_financial", "consolidated_financial_data"),
    debt_financial = "debt_financial_data",
    exchange_rates = "currencies",
    fin_sector_overrides = "fin_sector_overrides",
    non_distinct_isins = "non_distinct_isins",
    revenue = "revenue_data",
    sector_bridge = "sector_bridge",
    security_financial = "security_financial_data"
  )


sector_list <- c("Automotive", "Power", "Oil&Gas", "Coal")
other_sector_list <- c("Shipping", "Steel", "Aviation", "Cement")


cb_groups_vector <-
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
    cb_groups_vector
  }


get_sb_groups <-
  function() {
    sb_groups
  }


in_groups <-
  function(x, group) {
    x %in% do.call(group, args = list())
  }


in_cb_groups <-
  function(x) {
    in_groups(x, "cb_groups")
  }


in_sb_groups <-
  function(x) {
    in_groups(x, "sb_groups")
  }
