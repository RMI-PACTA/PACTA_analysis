# importing internal data ------------------------------------------------------

get_bics_bridge <-
  function(path = "inst/extdata", filename) {
    if (missing(filename)) {
      filename = paste0(default_filenames_sans_ext["bics_bridge"], ".rds")
    }
    data <- get_rds_data_from_path(path, filename)
    stopifnot(validate_bics_bridge(data))
    data
  }


get_exchange_rates <-
  function(path = "inst/extdata", filename) {
    if (missing(filename)) {
      filename = paste0(default_filenames_sans_ext["exchange_rates"], ".rds")
    }
    data <- get_rds_data_from_path(path, filename)
    stopifnot(validate_exchange_rates(data))
    data
  }


get_fin_sector_overrides <-
  function(path = "inst/extdata", filename) {
    if (missing(filename)) {
      filename = paste0(default_filenames_sans_ext["fin_sector_overrides"], ".rds")
    }
    data <- get_rds_data_from_path(path, filename)
    stopifnot(validate_fin_sector_overrides(data))
    data
  }


get_non_distinct_isins <-
  function(path = "inst/extdata", filename) {
    if (missing(filename)) {
      filename = paste0(default_filenames_sans_ext["non_distinct_isins"], ".rds")
    }
    data <- get_rds_data_from_path(path, filename)
    stopifnot(validate_non_distinct_isins(data))
    data
  }


get_sector_bridge <-
  function(path = "inst/extdata", filename) {
    if (missing(filename)) {
      filename = paste0(default_filenames_sans_ext["sector_bridge"], ".rds")
    }
    data <- get_rds_data_from_path(path, filename)
    stopifnot(validate_sector_bridge(data))
    data
  }



# importing data from an external directory ------------------------------------

get_average_sector_intensity <-
  function(path, filename) {
    if (missing(filename)) {
      filename = paste0(default_filenames_sans_ext["average_sector_intensity"], ".rds")
    }
    data <- get_rds_data_from_path(path, filename)
    stopifnot(validate_average_sector_intensity(data))
    data
  }


get_company_emissions <-
  function(path, filename) {
    if (missing(filename)) {
      filename = paste0(default_filenames_sans_ext["company_emissions"], ".rds")
    }
    data <- get_rds_data_from_path(path, filename)
    stopifnot(validate_company_emissions(data))
    data
  }


get_consolidated_financial <-
  function(path, filename) {
    if (missing(filename)) {
      filename = paste0(default_filenames_sans_ext["consolidated_financial"], ".rds")
    }
    data <- get_rds_data_from_path(path, filename)
    stopifnot(validate_consolidated_financial(data))
    data
  }


get_debt_financial <-
  function(path, filename) {
    if (missing(filename)) {
      filename = paste0(default_filenames_sans_ext["debt_financial"], ".rds")
    }
    data <- get_rds_data_from_path(path, filename)
    stopifnot(validate_debt_financial(data))
    data
  }


get_funds <-
  function(path, filename) {
    data <- get_rds_data_from_path(path, filename)
    stopifnot(validate_funds(data))
    data
  }


get_revenue <-
  function(path, filename) {
    if (missing(filename)) {
      filename = paste0(default_filenames_sans_ext["revenue"], ".rds")
    }
    data <- get_rds_data_from_path(path, filename)
    stopifnot(validate_revenue(data))
    data
  }


get_security_financial <-
  function(path, filename) {
    if (missing(filename)) {
      filename = paste0(default_filenames_sans_ext["security_financial"], ".rds")
    }
    data <- get_rds_data_from_path(path, filename)
    stopifnot(validate_security_financial(data))
    data
  }



# multi-stage data importing and modifying -------------------------------------

get_exchange_rates_for_timestamp <-
  function(financial_timestamp,
           exchange_rates = NULL,
           exchange_rate_colname = paste0("ExchangeRate_", financial_timestamp),
           currency_code_colname = "Currency_abbr") {
    if (is.null(exchange_rates)) {
      exchange_rates <- get_exchange_rates()
    }

    stopifnot(inherits(exchange_rates, "data.frame"))
    stopifnot(currency_code_colname %in% names(exchange_rates))
    stopifnot(exchange_rate_colname %in% names(exchange_rates))
    stopifnot(inherits(exchange_rates[[currency_code_colname]], "character"))
    stopifnot(inherits(exchange_rates[[exchange_rate_colname]], "numeric"))

    exchange_rates <- exchange_rates[, c(currency_code_colname, exchange_rate_colname)]
    names(exchange_rates) <- c("currency", "exchange_rate")
    exchange_rates <- exchange_rates[!is.na(exchange_rates$currency), ]
    exchange_rates <- exchange_rates[exchange_rates$currency != "", ]
    exchange_rates <- exchange_rates[!duplicated(exchange_rates), ]

    exchange_rates
  }


get_and_clean_company_fin_data <-
  function(path) {
    .data <- get_consolidated_financial(path)
    .data <- .data[c("company_id",
                     "company_name",
                     "bloomberg_id",
                     "country_of_domicile",
                     "corporate_bond_ticker",
                     "bics_subgroup",
                     "icb_subgroup",
                     "has_asset_level_data",
                     "has_assets_in_matched_sector",
                     "sectors_with_assets",
                     "current_shares_outstanding_all_classes",
                     "market_cap",
                     "financial_timestamp")]
    .data$sector = convert_bics_to_sector(.data$bics_subgroup)
    .data$sector_icb = convert_icb_to_sector(.data$icb_subgroup)
    .data$sector = ifelse(is.na(.data$sector), .data$sector_icb, .data$sector)
    .data$sector_icb <- NULL
    .data$financial_sector <- .data$sector
    .data$sector <- NULL
    .data[!is.na(.data$financial_sector), ]
  }


get_and_clean_fund_data <-
  function(path, filename) {
    .data <- NULL
    . <- NULL
    get_funds(path, filename) %>%
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
      dplyr::ungroup()
  }


get_and_clean_revenue_data <-
  function(path, filename) {
    if (missing(filename)) {
      revenue_data <- get_revenue(path)
    } else {
      revenue_data <- get_revenue(path, filename)
    }

    revenue_data <- revenue_data[revenue_data$tot_rev > 0, ]
    names(revenue_data)[names(revenue_data) == "sector"] <- "revenue_sector"
    return(revenue_data)
  }
