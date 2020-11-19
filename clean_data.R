get_rds_data_from_path <- function(path, filename) {
  filepath <- file.path(path, filename)
  if (tools::file_ext(filepath) != "rds") {
    stop(paste0("file does not have the appropriate 'rds' extension: ", filename))
  }
  if (!file.exists(filepath)) {
    stop(paste0("file does not exist: ", filepath))
  }
  tryCatch(
    readRDS(filepath),
    error = function(e) stop(paste0("file cannot be read with readRDS(): ", filepath))
  )
}



# -------------------------------------------------------------------------

get_currency_data <-
  function(path = "data", filename = "currencies.rds") {
    get_rds_data_from_path(path, filename)
  }


get_sector_bridge <-
  function(path = "data", filename = "sector_bridge.rds") {
    get_rds_data_from_path(path, filename)
  }



# -------------------------------------------------------------------------

get_average_sector_intensity_data <-
  function(path, filename = "average_sector_intensity.rds") {
    get_rds_data_from_path(path, filename)
  }


get_company_emissions_data <-
  function(path, filename = "company_emissions.rds") {
    get_rds_data_from_path(path, filename)
  }


get_consolidated_financial_data <-
  function(path, filename = "consolidated_financial_data.rds") {
    get_rds_data_from_path(path, filename)
  }


get_debt_fin_data <-
  function(path, filename = "debt_financial_data.rds") {
    get_rds_data_from_path(path, filename)
  }


get_revenue_data <-
  function(path, filename = "revenue_data_member_ticker.rds") {
    get_rds_data_from_path(path, filename)
  }



# -------------------------------------------------------------------------

get_currency_data_for_timestamp <-
  function(financial_timestamp,
           currency_data = NULL,
           exchange_rate_colname = paste0("ExchangeRate_", financial_timestamp),
           currency_code_colname = "Currency_abbr") {
    if (is.null(currency_data)) {
      currency_data <- get_currency_data()
    }

    stopifnot(inherits(currency_data, "data.frame"))
    stopifnot(currency_code_colname %in% names(currency_data))
    stopifnot(exchange_rate_colname %in% names(currency_data))
    stopifnot(inherits(currency_data[[currency_code_colname]], "character"))
    stopifnot(inherits(currency_data[[exchange_rate_colname]], "numeric"))

    currency_data <- currency_data[, c(currency_code_colname, exchange_rate_colname)]
    names(currency_data) <- c("currency", "exchange_rate")
    currency_data <- currency_data[!is.na(currency_data$currency), ]
    currency_data <- currency_data[currency_data$currency != "", ]
    currency_data <- currency_data[!duplicated(currency_data), ]

    currency_data
  }


get_and_clean_company_fin_data <-
  function(path) {
    output <- get_consolidated_financial_data(path)

    sector_bridge <-
      get_sector_bridge() %>%
      filter(source %in% c("BICS", "ICB")) %>%
      select(industry_classification, sector) %>%
      distinct()

    output %>%
      select(company_id, company_name, bloomberg_id, country_of_domicile,
             corporate_bond_ticker, bics_subgroup, icb_subgroup,
             has_asset_level_data, has_assets_in_matched_sector,
             sectors_with_assets, current_shares_outstanding_all_classes,
             market_cap, financial_timestamp) %>%
      left_join(sector_bridge,
                by = c("bics_subgroup" = "industry_classification")) %>%
      left_join(rename(sector_bridge, sector_icb = sector),
                by = c("icb_subgroup" = "industry_classification")) %>%
      mutate(sector = if_else(is.na(sector), sector_icb, sector)) %>%
      select(-sector_icb) %>%
      rename(financial_sector = sector) %>%
      filter(!is.na(financial_sector))
  }
