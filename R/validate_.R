# validate imported datasets ---------------------------------------------------

validate_column_names <-
  function(.data, columns) {
    stopifnot(validate_is_dataframe(.data))
    stopifnot(validate_is_named_character(columns))
    all(names(columns) %in% names(.data))
  }


validate_column_types <-
  function(.data, columns) {
    stopifnot(validate_is_dataframe(.data))
    stopifnot(validate_is_named_character(columns))
    all(sapply(seq_along(columns), function(i) {
      class(.data[[names(columns)[i]]]) == columns[i]
    }))
  }


validate_data_frame_with_more_than_0_rows <-
  function(.data) {
    inherits(.data, "data.frame") && nrow(.data) > 0
  }


validate_has_column_that_matches <-
  function(.data, regex, ...) {
    any(grepl(regex, names(.data), ...))
  }


validate_is_dataframe <-
  function(.data) {
    inherits(.data, "data.frame")
  }


validate_is_named_character <-
  function(.data) {
    class(.data) == "character" && !is.null(attr(.data, "names"))
  }


validate_only_column_names <-
  function(.data, columns) {
    all(names(.data) %in% names(columns))
  }


validate_ungrouped <-
  function(.data) {
    !dplyr::is_grouped_df(.data)
  }


validate_average_sector_intensity <-
  function(.data) {
    columns <- colspec_average_sector_intensity()

    all(
      validate_is_dataframe(.data),
      validate_ungrouped(.data),
      validate_column_names(.data, columns),
      validate_only_column_names(.data, columns),
      validate_column_types(.data, columns)
    )
  }


validate_bics_bridge <-
  function(.data) {
    columns <- colspec_bics_bridge()

    all(
      validate_is_dataframe(.data),
      validate_ungrouped(.data),
      validate_column_names(.data, columns),
      validate_only_column_names(.data, columns),
      validate_column_types(.data, columns),
      !any(duplicated(.data$bics_subgroup))
    )
  }


validate_company_emissions <-
  function(.data) {
    columns <- colspec_company_emissions()

    all(
      validate_is_dataframe(.data),
      validate_ungrouped(.data),
      validate_column_names(.data, columns),
      validate_only_column_names(.data, columns),
      validate_column_types(.data, columns)
    )
  }


validate_consolidated_financial <-
  function(.data) {
    columns <- colspec_consolidated_financial()

    all(
      validate_is_dataframe(.data),
      validate_ungrouped(.data),
      validate_column_names(.data, columns),
      validate_column_types(.data, columns)
    )
  }


validate_exchange_rates <-
  function(.data) {
    columns <- colspec_exchange_rates()

    all(
      validate_is_dataframe(.data),
      validate_ungrouped(.data),
      validate_column_names(.data, columns),
      validate_column_types(.data, columns),
      validate_has_column_that_matches(.data, "ExchangeRate_[12][09][0-9]{2}Q[1-4]")
    )
  }


validate_debt_financial <-
  function(.data) {
    columns <- colspec_debt_financial()

    all(
      validate_is_dataframe(.data),
      validate_ungrouped(.data),
      validate_column_names(.data, columns),
      validate_only_column_names(.data, columns),
      validate_column_types(.data, columns)
    )
  }


validate_fin_sector_overrides <-
  function(.data) {
    columns <- colspec_fin_sector_overrides()

    all(
      validate_is_dataframe(.data),
      validate_ungrouped(.data),
      validate_column_names(.data, columns),
      validate_only_column_names(.data, columns),
      validate_column_types(.data, columns)
    )
  }


validate_funds <-
  function(.data) {
    columns <- colspec_funds()

    all(
      validate_is_dataframe(.data),
      validate_ungrouped(.data),
      validate_column_names(.data, columns),
      validate_only_column_names(.data, columns),
      validate_column_types(.data, columns)
    )
  }


validate_non_distinct_isins <-
  function(.data) {
    columns <- colspec_non_distinct_isins()

    all(
      validate_is_dataframe(.data),
      validate_ungrouped(.data),
      validate_column_names(.data, columns),
      validate_only_column_names(.data, columns),
      validate_column_types(.data, columns)
    )
  }


validate_revenue <-
  function(.data) {
    columns <- colspec_revenue()

    all(
      validate_is_dataframe(.data),
      validate_ungrouped(.data),
      validate_column_names(.data, columns),
      validate_only_column_names(.data, columns),
      validate_column_types(.data, columns)
    )
  }


validate_sector_bridge <-
  function(.data) {
    columns <- colspec_sector_bridge()

    all(
      validate_is_dataframe(.data),
      validate_ungrouped(.data),
      validate_column_names(.data, columns),
      validate_only_column_names(.data, columns),
      validate_column_types(.data, columns),
      !any(duplicated(.data$industry_classification[.data$source == "BCLASS"])),
      !any(duplicated(.data$industry_classification[.data$source == "BICS"])),
      !any(duplicated(.data$industry_classification[.data$source == "ICB"]))
    )
  }


validate_security_financial <-
  function(.data) {
    columns <- colspec_security_financial()

    all(
      validate_is_dataframe(.data),
      validate_ungrouped(.data),
      validate_column_names(.data, columns),
      validate_column_types(.data, columns)
    )
  }


validate_cleaned_security_financial <-
  function(.data, financial_timestamp) {
    columns <- colspec_security_financial()
    non_distinct_isins <- get_non_distinct_isins()$isin
    allowable_sectors <- c(sector_list, other_sector_list, "Other")

    all(
      validate_is_dataframe(.data),
      validate_ungrouped(.data),
      validate_column_names(.data, columns),
      validate_column_types(.data, columns),
      all(!duplicated(.data)),
      validate_none_within(.data$isin, non_distinct_isins),
      length(unique(.data$financial_timestamp)) == 1,
      unique(.data$financial_timestamp) == financial_timestamp,
      validate_no_nas(.data$security_mapped_sector),
      validate_all_within(.data$security_mapped_sector, allowable_sectors),
      all(.data$asset_type != "Other"),
      validate_no_nas(.data$asset_type)
    )
  }


validate_no_nas <-
  function(.data) {
    all(!is.na(.data))
  }


validate_all_within <-
  function(.data, allowed) {
    all(.data %in% allowed)
  }


validate_none_within <-
  function(.data, allowed) {
    all(!.data %in% allowed)
  }
