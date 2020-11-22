# utilities --------------------------------------------------------------------

get_rds_data_from_path <-
  function(path, filename) {
    filepath <- file.path(path, filename)
    if (tools::file_ext(filepath) != "rds") {
      stop(paste0("file does not have the appropriate 'rds' extension: ", filename))
    }
    if (!file.exists(filepath)) {
      stop(paste0("file does not exist: ", filepath))
    }
    tryCatch(
      readRDS(filepath) %>% dplyr::as_tibble() %>% dplyr::ungroup(),
      error = function(e) stop(paste0("file cannot be read with readRDS(): ", filepath))
    )
  }


fast_match <-
  function(x, dict) {
    if (inherits(x, "data.frame") && ncol(x) == 1L) { x <- x[[1]] }

    stopifnot(inherits(x, "character"))
    stopifnot(inherits(dict, "data.frame"))
    stopifnot(ncol(dict) == 2L)
    stopifnot(inherits(dict[[1]], "character"))

    xfctr <- factor(x)
    matchidxs <- match(levels(xfctr), dict[[1]])
    matches <- dict[[2]][matchidxs]
    matches[as.numeric(xfctr)]
  }


save_files_to <-
  function(path, ...) {
    require(fst)

    if (!dir.exists(path)) { dir.create(path) }

    dots <- match.call(expand.dots = FALSE)$...

    lapply(dots, function(obj_sym) {
      obj_name <- deparse(obj_sym)
      if (exists(obj_name)) {
        filename <- paste0(obj_name, ".fst")
        fst::write_fst(base::get(obj_name), file.path(path, filename))
      }
    })

    if (any(file.size(list.files(path, full.names = T)) > 100e6)) {
      warning("File size exceeds what can be pushed to GitHub. Check before Committing")
    }

    invisible()
  }



# importing internal data ------------------------------------------------------

get_bics_bridge_data <-
  function(path = "data", filename = "bics_bridge.rds") {
    data <- get_rds_data_from_path(path, filename)
    stopifnot(validate_bics_bridge_data(data))
    data
  }


get_currency_data <-
  function(path = "data", filename = "currencies.rds") {
    data <- get_rds_data_from_path(path, filename)
    stopifnot(validate_currency_data(data))
    data
  }


get_fin_sector_overrides_data <-
  function(path = "data", filename = "fin_sector_overrides.rds") {
    data <- get_rds_data_from_path(path, filename)
    stopifnot(validate_fin_sector_overrides_data(data))
    data
  }


get_non_distinct_isins_data <-
  function(path = "data", filename = "non_distinct_isins.rds") {
    data <- get_rds_data_from_path(path, filename)
    stopifnot(validate_non_distinct_isins_data(data))
    data
  }


get_sector_bridge <-
  function(path = "data", filename = "sector_bridge.rds") {
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
    require(dplyr)

    get_consolidated_financial_data(path) %>%
      select(company_id, company_name, bloomberg_id, country_of_domicile,
             corporate_bond_ticker, bics_subgroup, icb_subgroup,
             has_asset_level_data, has_assets_in_matched_sector,
             sectors_with_assets, current_shares_outstanding_all_classes,
             market_cap, financial_timestamp) %>%
      mutate(sector = sector_from_bics_subgroup(bics_subgroup)) %>%
      mutate(sector_icb = sector_from_icb_subgroup(icb_subgroup)) %>%
      mutate(sector = if_else(is.na(sector), sector_icb, sector)) %>%
      select(-sector_icb) %>%
      rename(financial_sector = sector) %>%
      filter(!is.na(financial_sector))
  }


get_and_clean_fund_data <-
  function(path, filename) {
    get_fund_data(path, filename) %>%
      filter(!is.na(holding_isin) & holding_isin != "") %>%
      group_by(fund_isin) %>%
      mutate(total_weight = sum(isin_weight, na.rm = T)) %>%
      mutate(isin_weight = if_else(total_weight > 1, isin_weight / total_weight, isin_weight)) %>%
      bind_rows(filter(., total_weight <= 1) %>%
                  summarise(holding_isin = "MissingValue",
                            isin_weight = 1 - sum(isin_weight, na.rm = T),
                            .groups = "drop")) %>%
      select(-total_weight) %>%
      ungroup() %>%
      arrange(fund_isin, holding_isin != "MissingValue", holding_isin)
  }



# data merging -----------------------------------------------------------------

convert_industry_classification <-
  function(data, from, to) {
    sector_bridge <- get_sector_bridge()
    stopifnot(to %in% names(sector_bridge))

    dict <- sector_bridge[sector_bridge$source == from, ]
    dict <- dict[c("industry_classification", to)]
    fast_match(x = data, dict = dict)
  }


sector_from_bics_subgroup <-
  function(data) {
    convert_industry_classification(data, from = "BICS", to = "sector")
  }


sector_from_icb_subgroup <-
  function(data) {
    convert_industry_classification(data, from = "ICB", to = "sector")
  }



# validate imported datasets ---------------------------------------------------

validate_column_names <-
  function(data, columns) {
    all(names(columns) %in% names(data))
  }


validate_column_types <-
  function(data, columns) {
    all(sapply(seq_along(columns), function(i) {
      class(data[[names(columns)[i]]]) == columns[i]
    }))
  }


validate_has_column_that_matches <-
  function(data, regex, ...) {
    any(grepl(regex, names(data), ...))
  }


validate_is_dataframe <-
  function(data) {
    inherits(data, "data.frame")
  }


validate_only_column_names <-
  function(data, columns) {
    all(names(data) %in% names(columns))
  }


validate_ungrouped <-
  function(data) {
    !dplyr::is_grouped_df(data)
  }


validate_average_sector_intensity_data <-
  function(data) {
    columns <- c(
      bics_sector = "character",
      mean_intensity = "numeric",
      median_intensity = "numeric",
      sd_intensity = "numeric",
      asset_type = "character",
      unit = "character"
    )

    all(
      validate_is_dataframe(data),
      validate_ungrouped(data),
      validate_column_names(data, columns),
      validate_only_column_names(data, columns),
      validate_column_types(data, columns)
    )
  }


validate_bics_bridge_data <-
  function(data) {
    columns <- c(
      bics_sector = "character",
      bics_subgroup = "character"
    )

    all(
      validate_is_dataframe(data),
      validate_ungrouped(data),
      validate_column_names(data, columns),
      validate_only_column_names(data, columns),
      validate_column_types(data, columns)
    )
  }


validate_company_emissions_data <-
  function(data) {
    columns <- c(
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

    all(
      validate_is_dataframe(data),
      validate_ungrouped(data),
      validate_column_names(data, columns),
      validate_only_column_names(data, columns),
      validate_column_types(data, columns)
    )
  }


validate_consolidated_financial_data <-
  function(data) {
    columns <- c(
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

    all(
      validate_is_dataframe(data),
      validate_ungrouped(data),
      validate_column_names(data, columns),
      validate_only_column_names(data, columns),
      validate_column_types(data, columns)
    )
  }


validate_currency_data <-
  function(data) {
    columns <- c(
      Currency = "character",
      Currency_abbr = "character"
    )

    all(
      validate_is_dataframe(data),
      validate_ungrouped(data),
      validate_column_names(data, columns),
      validate_column_types(data, columns),
      validate_has_column_that_matches(data, "ExchangeRate_[12][09][0-9]{2}Q[1-4]")
    )
  }


validate_debt_financial_data <-
  function(data) {
    columns <- c(
      corporate_bond_ticker = "character",
      has_asset_level_data = "logical",
      sectors_with_assets = "character",
      bics_sector = "character"
    )

    all(
      validate_is_dataframe(data),
      validate_ungrouped(data),
      validate_column_names(data, columns),
      validate_only_column_names(data, columns),
      validate_column_types(data, columns)
    )
  }


validate_fin_sector_overrides_data <-
  function(data) {
    columns <- c(
      company_name = "character",
      corporate_bond_ticker = "character",
      bloomberg_id = "numeric",
      fin_sector_override = "character"
    )

    all(
      validate_is_dataframe(data),
      validate_ungrouped(data),
      validate_column_names(data, columns),
      validate_only_column_names(data, columns),
      validate_column_types(data, columns)
    )
  }


validate_fund_data <-
  function(data) {
    columns <- c(
      fund_isin = "character",
      holding_isin = "character",
      isin_weight = "numeric",
      fund_type = "character"
    )

    all(
      validate_is_dataframe(data),
      validate_ungrouped(data),
      validate_column_names(data, columns),
      validate_only_column_names(data, columns),
      validate_column_types(data, columns)
    )
  }


validate_non_distinct_isins_data <-
  function(data) {
    columns <- c(
      isin = "character"
    )

    all(
      validate_is_dataframe(data),
      validate_ungrouped(data),
      validate_column_names(data, columns),
      validate_only_column_names(data, columns),
      validate_column_types(data, columns)
    )
  }


validate_revenue_data <-
  function(data) {
    columns <- c(
      company_id = "numeric",
      company_name = "character",
      bloomberg_id = "numeric",
      equity_ticker = "character",
      corporate_bond_ticker = "character",
      has_revenue_data = "logical",
      sector = "character",
      tot_rev = "numeric"
    )

    all(
      validate_is_dataframe(data),
      validate_ungrouped(data),
      validate_column_names(data, columns),
      validate_only_column_names(data, columns),
      validate_column_types(data, columns)
    )
  }


validate_sector_bridge <-
  function(data) {
    columns <- c(
      industry_classification = "character",
      source = "character",
      sector = "character",
      sector_ipr = "character",
      subsector_ipr = "character",
      sector_dnb = "character",
      sector_boe = "character",
      subsector_boe = "character"
    )

    all(
      validate_is_dataframe(data),
      validate_ungrouped(data),
      validate_column_names(data, columns),
      validate_only_column_names(data, columns),
      validate_column_types(data, columns)
    )
  }


validate_security_financial_data <-
  function(data) {
    columns <- c(
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

    all(
      validate_is_dataframe(data),
      validate_ungrouped(data),
      validate_column_names(data, columns),
      validate_only_column_names(data, columns),
      validate_column_types(data, columns)
    )
  }
