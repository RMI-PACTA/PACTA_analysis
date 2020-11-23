# importing internal data ------------------------------------------------------

get_bics_bridge_data <-
  function(path = "inst/extdata", filename = "bics_bridge.rds") {
    data <- get_rds_data_from_path(path, filename)
    stopifnot(validate_bics_bridge_data(data))
    data
  }


get_currency_data <-
  function(path = "inst/extdata", filename = "currencies.rds") {
    data <- get_rds_data_from_path(path, filename)
    stopifnot(validate_currency_data(data))
    data
  }


get_fin_sector_overrides_data <-
  function(path = "inst/extdata", filename = "fin_sector_overrides.rds") {
    data <- get_rds_data_from_path(path, filename)
    stopifnot(validate_fin_sector_overrides_data(data))
    data
  }


get_non_distinct_isins_data <-
  function(path = "inst/extdata", filename = "non_distinct_isins.rds") {
    data <- get_rds_data_from_path(path, filename)
    stopifnot(validate_non_distinct_isins_data(data))
    data
  }


get_sector_bridge <-
  function(path = "inst/extdata", filename = "sector_bridge.rds") {
    data <- get_rds_data_from_path(path, filename)
    stopifnot(validate_sector_bridge(data))
    data
  }



# importing data from an external directory ------------------------------------

get_average_sector_intensity_data <-
  function(path, filename = "average_sector_intensity.rds") {
    data <- get_rds_data_from_path(path, filename)
    stopifnot(validate_average_sector_intensity_data(data))
    data
  }


get_company_emissions_data <-
  function(path, filename = "company_emissions.rds") {
    data <- get_rds_data_from_path(path, filename)
    stopifnot(validate_company_emissions_data(data))
    data
  }


get_consolidated_financial_data <-
  function(path, filename = "consolidated_financial_data.rds") {
    data <- get_rds_data_from_path(path, filename)
    stopifnot(validate_consolidated_financial_data(data))
    data
  }


get_debt_financial_data <-
  function(path, filename = "debt_financial_data.rds") {
    data <- get_rds_data_from_path(path, filename)
    stopifnot(validate_debt_financial_data(data))
    data
  }


get_fund_data <-
  function(path, filename) {
    data <- get_rds_data_from_path(path, filename)
    stopifnot(validate_fund_data(data))
    data
  }


get_revenue_data <-
  function(path, filename = "revenue_data.rds") {
    data <- get_rds_data_from_path(path, filename)
    stopifnot(validate_revenue_data(data))
    data
  }


get_security_financial_data <-
  function(path, filename = "security_financial_data.rds") {
    data <- get_rds_data_from_path(path, filename)
    stopifnot(validate_security_financial_data(data))
    data
  }



# multi-stage data importing and modifying -------------------------------------

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
    .data <- NULL
    get_consolidated_financial_data(path) %>%
      dplyr::select(
        .data$company_id,
        .data$company_name,
        .data$bloomberg_id,
        .data$country_of_domicile,
        .data$corporate_bond_ticker,
        .data$bics_subgroup,
        .data$icb_subgroup,
        .data$has_asset_level_data,
        .data$has_assets_in_matched_sector,
        .data$sectors_with_assets,
        .data$current_shares_outstanding_all_classes,
        .data$market_cap,
        .data$financial_timestamp
      ) %>%
      dplyr::mutate(sector = convert_bics_to_sector(.data$bics_subgroup)) %>%
      dplyr::mutate(sector_icb = convert_icb_to_sector(.data$icb_subgroup)) %>%
      dplyr::mutate(sector = dplyr::if_else(is.na(.data$sector), .data$sector_icb, .data$sector)) %>%
      dplyr::select(-.data$sector_icb) %>%
      dplyr::rename(financial_sector = .data$sector) %>%
      dplyr::filter(!is.na(.data$financial_sector))
  }


get_and_clean_fund_data <-
  function(path, filename) {
    .data <- NULL
    . <- NULL
    get_fund_data(path, filename) %>%
      dplyr::filter(!is.na(.data$holding_isin) &
                      .data$holding_isin != "") %>%
      dplyr::group_by(.data$fund_isin) %>%
      dplyr::mutate(total_weight = sum(.data$isin_weight, na.rm = T)) %>%
      dplyr::mutate(
        isin_weight = dplyr::if_else(
          .data$total_weight > 1,
          .data$isin_weight / .data$total_weight,
          .data$isin_weight
        )
      ) %>%
      dplyr::bind_rows(
        dplyr::filter(., .data$total_weight <= 1) %>%
          dplyr::summarise(
            holding_isin = "MissingValue",
            isin_weight = 1 - sum(.data$isin_weight, na.rm = T),
            .groups = "drop"
          )
      ) %>%
      dplyr::select(-.data$total_weight) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(.data$fund_isin,
                     .data$holding_isin != "MissingValue",
                     .data$holding_isin)
  }
