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
      readRDS(filepath),
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
    if (!dir.exists(path)) {
      dir.create(path)
    }

    dots <- match.call(expand.dots = FALSE)$...

    lapply(dots, function(obj_name) {
      obj <- get(deparse(obj_name))
      filename <- paste0(obj_name, ".fst")
      fst::write_fst(obj, file.path(path, filename))
    })

    invisible()
  }



# importing internal data ------------------------------------------------------

get_bics_bridge_data <-
  function(path = "data", filename = "bics_bridge.rds") {
    get_rds_data_from_path(path, filename)
  }


get_currency_data <-
  function(path = "data", filename = "currencies.rds") {
    get_rds_data_from_path(path, filename)
  }


get_fin_sector_overrides_data <-
  function(path = "data", filename = "fin_sector_overrides.rds") {
    get_rds_data_from_path(path, filename)
  }


get_non_distinct_isins_data <-
  function(path = "data", filename = "non_distinct_isins.rds") {
    get_rds_data_from_path(path, filename)
  }


get_sector_bridge <-
  function(path = "data", filename = "sector_bridge.rds") {
    get_rds_data_from_path(path, filename)
  }



# importing data from an external directory ------------------------------------

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


get_debt_financial_data <-
  function(path, filename = "debt_financial_data.rds") {
    get_rds_data_from_path(path, filename)
  }


get_revenue_data <-
  function(path, filename = "revenue_data_member_ticker.rds") {
    get_rds_data_from_path(path, filename)
  }


get_security_financial_data <-
  function(path, filename = "security_financial_data.rds") {
    get_rds_data_from_path(path, filename)
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
  function(financial_timestamp, path, filename = NULL) {
    if (is.null(filename)) {
      filename <- c(paste0("fund_data_", financial_timestamp, ".rds"),
                    "fund_data_2018Q4.rds", "SFC_26052020_funds.rds")
    }

    filename <- purrr::detect(filename, ~ file.exists(file.path(path, .x)))
    if (is.null(filename)) stop("a fund data file was not found")

    fund_data <- readRDS(file.path(path, filename))

    fund_data <- fund_data %>% janitor::clean_names()
    fund_data <- fund_data %>% filter(!is.na(holding_isin) & holding_isin != "")
    fund_data <- normalise_fund_data(fund_data)
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
