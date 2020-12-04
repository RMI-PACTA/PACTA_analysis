# validate imported datasets ---------------------------------------------------

new_error_collector <-
  function() {
    error_msgs <- list()
    context <- character(0L)
    push <- function(msg, details = NULL) error_msgs[[length(error_msgs) + 1]] <<- list(msg = msg, details = details)
    getMessages <- function() error_msgs
    isEmpty <- function() length(error_msgs) == 0L
    report <-
      function() {
        if (!isEmpty()) {
          errors <- getMessages()
          if (length(context) > 0) cli::cli_text(context)
          cli::cli_alert_danger("Failed on {length(errors)} assertion{?s}")
          cli::cli_ol()
          for (error in  errors) {
            cli::cli_li(error$msg)
            detail_list <- cli::cli_ul()
            for (detail in error$details) {
              cli::cli_li(detail)
            }
            cli::cli_end(detail_list)
          }
          cli::cli_end()
        }
        invisible(isEmpty())
      }
    structure(class = "error_collector", environment())
  }


validate_column_names <-
  function(.data, columns, error_collector = NULL) {
    stopifnot(validate_is_dataframe(.data))
    stopifnot(validate_is_named_character(columns))

    test <- names(columns) %in% names(.data)

    if (!all(test) && !is.null(error_collector)) {
      msg <- "required column names were not found in the data"
      error_collector$push(msg, names(columns)[!test])
    }

    return(all(test))
  }


validate_column_types <-
  function(.data, columns, error_collector = NULL) {
    stopifnot(validate_is_dataframe(.data))
    stopifnot(validate_is_named_character(columns))

    columns <- columns[names(columns) %in% names(.data)]

    test <-
      sapply(seq_along(columns), function(i) {
        class(.data[[names(columns)[i]]]) == columns[i]
      })

    if (!all(test) && !is.null(error_collector)) {
      msg <- "some column types do not match the specification"
      wrong_cols <- columns[!test]
      details <-
        sapply(seq_along(wrong_cols), function(i) {
          paste0(names(wrong_cols)[i], ": class is ", class(.data[[names(wrong_cols)[i]]]), " instead of ", wrong_cols[i])
        })
      error_collector$push(msg, details)
    }

    return(all(test))
  }


validate_data_frame_with_more_than_0_rows <-
  function(.data, error_collector = NULL) {
    test <- inherits(.data, "data.frame") && nrow(.data) > 0

    if (!all(test) && !is.null(error_collector)) {
      error_collector$push("object is not a data frame with more than 0 rows")
    }

    return(all(test))
  }


validate_has_column_that_matches <-
  function(.data, regex, ..., error_collector = NULL) {
    test <- grepl(regex, names(.data), ...)

    if (!any(test) && !is.null(error_collector)) {
      msg <- paste0("data frame does not have a column that matches \"", regex, "\"")
      error_collector$push(msg)
    }

    return(any(test))
  }


validate_is_dataframe <-
  function(.data, error_collector = NULL) {
    test <- inherits(.data, "data.frame")

    if (!all(test) && !is.null(error_collector)) {
      error_collector$push("object is not a data frame")
    }

    return(all(test))
  }


validate_is_named_character <-
  function(.data, error_collector = NULL) {
    test <- class(.data) == "character" && !is.null(attr(.data, "names"))

    if (!all(test) && !is.null(error_collector)) {
      error_collector$push("object is not a named character vector")
    }

    return(all(test))
  }


validate_only_column_names <-
  function(.data, columns, error_collector = NULL) {
    test <- names(.data) %in% names(columns)

    if (!all(test) && !is.null(error_collector)) {
      msg <- "the data frame has columns that are not in the specification"
      error_collector$push(msg, details = names(.data)[!test])
    }

    return(all(test))
  }


validate_ungrouped <-
  function(.data, error_collector = NULL) {
    test <- !dplyr::is_grouped_df(.data)

    if (!all(test) && !is.null(error_collector)) {
      error_collector$push("object is not an ungrouped data frame/tibble")
    }

    return(all(test))
  }


validate_average_sector_intensity <-
  function(.data) {
    columns <- colspec_average_sector_intensity()

    error_collector <- new_error_collector()
    error_collector$context <- "While running {.emph validate_average_sector_intensity}"

    validate_is_dataframe(.data, error_collector)
    validate_ungrouped(.data, error_collector)
    validate_column_names(.data, columns, error_collector)
    validate_only_column_names(.data, columns, error_collector)
    validate_column_types(.data, columns, error_collector)

    error_collector$report()
  }


validate_bics_bridge <-
  function(.data) {
    columns <- colspec_bics_bridge()

    error_collector <- new_error_collector()
    error_collector$context <- "While running {.emph validate_bics_bridge}"

    validate_is_dataframe(.data, error_collector)
    validate_ungrouped(.data, error_collector)
    validate_column_names(.data, columns, error_collector)
    validate_only_column_names(.data, columns, error_collector)
    validate_column_types(.data, columns, error_collector)

    if (any(duplicated(.data$bics_subgroup))) {
      error_collector$push("there are duplicated values in bics_subgroup")
    }

    error_collector$report()
  }


validate_company_emissions <-
  function(.data) {
    columns <- colspec_company_emissions()

    error_collector <- new_error_collector()
    error_collector$context <- "While running {.emph validate_company_emissions}"

    validate_is_dataframe(.data, error_collector)
    validate_ungrouped(.data, error_collector)
    validate_column_names(.data, columns, error_collector)
    validate_only_column_names(.data, columns, error_collector)
    validate_column_types(.data, columns, error_collector)

    error_collector$report()
  }


validate_consolidated_financial <-
  function(.data) {
    columns <- colspec_consolidated_financial()

    error_collector <- new_error_collector()
    error_collector$context <- "While running {.emph validate_consolidated_financial}"

    validate_is_dataframe(.data, error_collector)
    validate_ungrouped(.data, error_collector)
    validate_column_names(.data, columns, error_collector)
    validate_column_types(.data, columns, error_collector)

    error_collector$report()
  }


validate_exchange_rates <-
  function(.data) {
    columns <- colspec_exchange_rates()

    error_collector <- new_error_collector()
    error_collector$context <- "While running {.emph validate_exhange_rates}"

    validate_is_dataframe(.data, error_collector)
    validate_ungrouped(.data, error_collector)
    validate_column_names(.data, columns, error_collector)
    validate_column_types(.data, columns, error_collector)
    validate_has_column_that_matches(.data, regex = "ExchangeRate_[12][09][0-9]{2}Q[1-4]", error_collector = error_collector)

    return(error_collector$report())
  }


validate_debt_financial <-
  function(.data) {
    columns <- colspec_debt_financial()

    error_collector <- new_error_collector()
    error_collector$context <- "While running {.emph validate_debt_financial}"

    validate_is_dataframe(.data, error_collector)
    validate_ungrouped(.data, error_collector)
    validate_column_names(.data, columns, error_collector)
    validate_only_column_names(.data, columns, error_collector)
    validate_column_types(.data, columns, error_collector)

    return(error_collector$report())
  }


validate_fin_sector_overrides <-
  function(.data) {
    columns <- colspec_fin_sector_overrides()

    error_collector <- new_error_collector()
    error_collector$context <- "While running {.emph validate_fin_sector_overrides}"

    validate_is_dataframe(.data, error_collector)
    validate_ungrouped(.data, error_collector)
    validate_column_names(.data, columns, error_collector)
    validate_only_column_names(.data, columns, error_collector)
    validate_column_types(.data, columns, error_collector)

    return(error_collector$report())
  }


validate_funds <-
  function(.data) {
    columns <- colspec_funds()

    error_collector <- new_error_collector()
    error_collector$context <- "While running {.emph validate_funds}"

    validate_is_dataframe(.data, error_collector)
    validate_ungrouped(.data, error_collector)
    validate_column_names(.data, columns, error_collector)
    validate_only_column_names(.data, columns, error_collector)
    validate_column_types(.data, columns, error_collector)

    return(error_collector$report())
  }


validate_non_distinct_isins <-
  function(.data) {
    columns <- colspec_non_distinct_isins()

    error_collector <- new_error_collector()
    error_collector$context <- "While running {.emph validate_non_distinct_isins}"

    validate_is_dataframe(.data, error_collector)
    validate_ungrouped(.data, error_collector)
    validate_column_names(.data, columns, error_collector)
    validate_only_column_names(.data, columns, error_collector)
    validate_column_types(.data, columns, error_collector)

    return(error_collector$report())
  }


validate_revenue <-
  function(.data) {
    columns <- colspec_revenue()

    error_collector <- new_error_collector()
    error_collector$context <- "While running {.emph validate_revenue}"

    validate_is_dataframe(.data, error_collector)
    validate_ungrouped(.data, error_collector)
    validate_column_names(.data, columns, error_collector)
    validate_only_column_names(.data, columns, error_collector)
    validate_column_types(.data, columns, error_collector)

    return(error_collector$report())
  }


validate_sector_bridge <-
  function(.data) {
    columns <- colspec_sector_bridge()

    error_collector <- new_error_collector()
    error_collector$context <- "While running {.emph validate_sector_bridge}"

    validate_is_dataframe(.data, error_collector)
    validate_ungrouped(.data, error_collector)
    validate_column_names(.data, columns, error_collector)
    validate_only_column_names(.data, columns, error_collector)
    validate_column_types(.data, columns, error_collector)

    if (any(duplicated(.data$industry_classification[.data$source == "BCLASS"]))) {
      error_collector$push("there are duplicated values in industry_classification where source == \"BCLASS\"")
    }
    if (any(duplicated(.data$industry_classification[.data$source == "BICS"]))) {
      error_collector$push("there are duplicated values in industry_classification where source == \"BICS\"")
    }
    if (any(duplicated(.data$industry_classification[.data$source == "ICB"]))) {
      error_collector$push("there are duplicated values in industry_classification where source == \"ICB\"")
    }

    return(error_collector$report())
  }


validate_security_financial <-
  function(.data) {
    columns <- colspec_security_financial()

    error_collector <- new_error_collector()
    error_collector$context <- "While running {.emph validate_security_financial}"

    validate_is_dataframe(.data, error_collector)
    validate_ungrouped(.data, error_collector)
    validate_column_names(.data, columns, error_collector)
    validate_column_types(.data, columns, error_collector)

    return(error_collector$report())
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
  function(.data, error_collector = NULL) {
    test <- all(!is.na(.data))

    if (!all(test) && !is.null(error_collector)) {
      error_collector$push("the object contains NAs")
    }

    return(all(test))
  }


validate_all_within <-
  function(.data, allowed, error_collector = NULL) {
    test <- .data %in% allowed

    if (!all(test) && !is.null(error_collector)) {
      msg <- "the object contains values that are not within the allowed set"
      error_collector$push(msg, details = .data[!test])
    }

    return(all(test))
  }


validate_none_within <-
  function(.data, allowed, error_collector = NULL) {
    test <- !.data %in% allowed

    if (!all(test) && !is.null(error_collector)) {
      msg <- "the object contains values that are within the set of unallowed values"
      error_collector$push(msg, details = .data[!test])
    }

    return(all(test))
  }


validate_by_name <-
  function(name, .data) {
    do.call(paste0("validate_", name), args = list(.data))
  }
