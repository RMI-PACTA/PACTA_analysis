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
      dplyr::ungroup(dplyr::as_tibble(readRDS(filepath))),
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


first_char_up <-
  function(x) {
    paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))
  }


save_files_to <-
  function(path, ...) {
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

    invisible(TRUE)
  }



# copy-pasted from modified get_and_clean_fin_data process ---------------------

check_asset_types <-
  function(fin_data) {
    fin_data$asset_type[fin_data$asset_type == "Other"] <- "Others"
    fin_data$asset_type[is.na(fin_data$asset_type)] <- "Others"
    fin_data$asset_type <- first_char_up(fin_data$asset_type)
    return(fin_data)
  }


check_fin_mapped_sectors <-
  function(.data, sector_list, other_sector_list) {
    .data$security_mapped_sector[.data$security_mapped_sector == "Others"] <- "Other"
    .data$security_mapped_sector[.data$security_mapped_sector == "OIl&Gas"] <- "Oil&Gas"
    .data$security_mapped_sector[is.na(.data$security_mapped_sector)] <- "Other"

    actual_sectors <- unique(.data$security_mapped_sector)
    allowable_sectors <- c(sector_list, other_sector_list, "Other")
    if (!all(actual_sectors %in% allowable_sectors)) {
      stop("Additional Sectors in fin_data")
    }

    return(.data)
  }


convert_corporate_bonds <-
  function(.data) {
    .data$asset_type[in_cb_groups(.data$security_type)] <- "Bonds"
    .data$asset_type[!in_cb_groups(.data$security_type) & .data$asset_type == "Others"] <- "Bonds"
    return(.data)
  }


add_is_sb <-
  function(.data) {
    .data$is_sb <- in_sb_groups(.data$security_type) | in_sb_groups(.data$security_bics_subgroup)
    return(.data)
  }


classify_all_funds <-
  function(.data) {
    fund_rows <-
      grepl("Fund|ETF", .data$security_type) |
      grepl("Fund|ETF", .data$security_bclass4) |
      grepl("Fund|ETF", .data$security_icb_subsector)

    .data$asset_type[fund_rows] <- "Funds"
    return(.data)
  }


check_funds_wo_bbg <-
  function(fund_data, fin_data) {
    fin_data_funds <- unique(fin_data$isin[fin_data$asset_type == "Funds"])
    fund_isins <- unique(fund_data$fund_isin)
    fund_isins_missing_bbg <- fund_isins[!fund_isins %in% fin_data_funds]

    known_missing_isins <- readRDS("inst/extdata/fund_isins_without_bbg_data.rds")
    known_missing_isins <- unique(c(known_missing_isins, fund_isins_missing_bbg))

    if (length(fund_isins_missing_bbg) > 0) {
      warning("Warning: There are funds without bbg data. These are excluded from the analysis.")
    }
  }



# unmodified  -------------------------------------------------------------

map_security_sectors <-
  function(fin_data, sector_bridge) {
    initial_no_rows <- nrow(fin_data)

    fin_data <-
      fin_data %>%
      left_join(sector_bridge %>% filter(source == "BICS") %>% select(-source),
                by = c("security_bics_subgroup" = "industry_classification")
      ) %>%
      mutate(security_icb_subsector = as.character(security_icb_subsector))

    fin_data_na <-
      fin_data %>%
      filter(is.na(sector)) %>%
      select(-c(sector,sector_boe,subsector_boe,sector_dnb,sector_ipr,subsector_ipr))

    fin_data <- fin_data[!is.na(fin_data$sector), ]

    fin_data_na <-
      fin_data_na %>%
      left_join(
        subset(sector_bridge[sector_bridge$source == "ICB", ], select = -c(source)),
        by = c("security_icb_subsector" = "industry_classification")
      )

    fin_data <- bind_rows(fin_data, fin_data_na)

    fin_data$security_mapped_sector <- fin_data$sector
    fin_data$sector <- NULL

    fin_data %>%
      group_by(security_mapped_sector) %>%
      filter(is.na(security_mapped_sector)) %>%
      summarise(count = n())

    fin_data_na <- fin_data[is.na(fin_data$security_mapped_sector), ]

    if (nrow(fin_data) != initial_no_rows) {
      stop("Rows being dropped in mapping sectors")
    }

    return(fin_data)
  }


override_sector_classification <-
  function(fin_data, overrides) {
    start_rows <- nrow(fin_data)

    overrides$company_name <- as.character(overrides$company_name)
    overrides$corporate_bond_ticker <- as.character(overrides$corporate_bond_ticker)
    overrides$fin_sector_override <- as.character(overrides$fin_sector_override)

    overrides$sector_override <- TRUE

    # Merge in by company corp ticker
    overrides_cbt <-
      overrides %>%
      filter(corporate_bond_ticker != "", !is.na(corporate_bond_ticker)) %>%
      select(corporate_bond_ticker, fin_sector_override, sector_override) %>%
      distinct()

    fin_data <- left_join(fin_data, overrides_cbt, by = "corporate_bond_ticker")

    # Merge in by bloomberg_id
    overrides_bbg <-
      overrides %>%
      filter(is.na(corporate_bond_ticker) | corporate_bond_ticker == "") %>%
      select(bloomberg_id, fin_sector_override, sector_override) %>%
      distinct()

    fin_data <- left_join(fin_data, overrides_bbg, by = "bloomberg_id")

    # Clean resulting financial data
    fin_data <-
      fin_data %>%
      mutate(
        sector_override = sector_override.x,
        sector_override = if_else(sector_override.y != "" & !is.na(sector_override.y), sector_override.y, sector_override),
        fin_sector_override = fin_sector_override.x,
        fin_sector_override = if_else(!is.na(fin_sector_override.y) & fin_sector_override.y != "", fin_sector_override.y, fin_sector_override),
        sector_override = if_else(is.na(sector_override), FALSE, TRUE)
      ) %>%
      select(-sector_override.x, -sector_override.y, -fin_sector_override.x, -fin_sector_override.y)

    fin_data <-
      fin_data %>%
      mutate(security_mapped_sector = if_else(sector_override, fin_sector_override, security_mapped_sector)) %>%
      select(-fin_sector_override)

    if (nrow(fin_data) != start_rows) {
      stop("Additional rows being added by fin sector override")
    }

    return(fin_data)
  }


get_and_clean_fin_data <-
  function(path, filename, financial_timestamp) {
    sec_fin <- get_security_financial(path, filename)

    non_distinct_isins <- get_non_distinct_isins()
    sec_fin <- sec_fin[!sec_fin$isin %in% non_distinct_isins$isin, ]

    if (!unique(sec_fin$financial_timestamp) == financial_timestamp) {
      message("Financial timestamp not equal")
    }

    orig_num_rows <- nrow(sec_fin)

    sec_fin <- sec_fin[!is.na(sec_fin$isin), ]

    sec_fin <- map_security_sectors(sec_fin, get_sector_bridge())

    # Adds in the manual sector classification overrides
    overrides <- get_fin_sector_overrides()
    sec_fin <- override_sector_classification(sec_fin, overrides)

    # Checks that only eq, cb, funds and others are in the fin_data
    sec_fin <- check_asset_types(sec_fin)

    # Checks for other mapped sectors not within the sector lists
    sec_fin <- check_fin_mapped_sectors(sec_fin, sector_list, other_sector_list)

    # TODO: find alternative here, calling in data from company financial data
    # Cleans and normalises the mapped_to_assets flag
    # fin_data <- check_mapped_assets_flag(fin_data)

    # Limits the Bonds category to corporate bonds only
    sec_fin <- convert_corporate_bonds(sec_fin)

    # Checks whether the bond is sovereign or not
    sec_fin <- add_is_sb(sec_fin)

    # Checks to ensure all finds are classified as such
    sec_fin <- classify_all_funds(sec_fin)

    sec_fin <- mutate(sec_fin, bics_sector = convert_bics_subgroup_to_bics_sector(security_bics_subgroup))
    # security_financial_data_out <- add_bics_sector(security_financial_data_out)

    # Select relevant columns
    sec_fin <-
      sec_fin %>%
      dplyr::select(
        .data$company_id,
        .data$company_name,
        .data$bloomberg_id,
        .data$corporate_bond_ticker,
        .data$country_of_domicile,
        .data$isin,
        .data$unit_share_price,
        # .data$exchange_rate_usd,
        .data$asset_type,
        .data$security_type,
        .data$security_mapped_sector,
        .data$security_icb_subsector,
        .data$security_bics_subgroup,
        .data$bics_sector,
        # .data$bclass4,
        # .data$maturity_date,
        # .data$coupon_value,
        # .data$amount_issued,
        .data$current_shares_outstanding_all_classes,
        .data$unit_share_price,
        .data$sector_override,
        .data$sector_boe,
        .data$subsector_boe,
        .data$sector_dnb,
        .data$sector_ipr,
        .data$subsector_ipr,
        .data$is_sb
      ) %>%
      dplyr::distinct()

    ### TEST
    if (nrow(sec_fin) > orig_num_rows) {
      stop("Additional rows added to fin data")
    }

    return(sec_fin)
  }

