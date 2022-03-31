#' Read in portfolio CSV/s, working around a number of non-standard issues
#'
#' This function will read in one more portfolio CSVs. It works around a number
#' of common issues, like alternate column names, alternate delimiter,
#' alternate decimal and grouping marks, file encodings besides ASCII or UTF-8,
#' etc.
#'
#' @param filepaths A character vector or single column data frame (strings
#'   should be valid file paths to CSV files or a directory that contains CSV
#'   files)
#' @param combine A single element logical (default `TRUE`)
#'
#' @return If `combine` is `TRUE`, returns a tbl_df with all of the readable
#'   data from the portfolio CSVs combined. If `combine` is `FALSE`, returns a
#'   list of tbl_dfs, one for each readable portfolio CSV.
#'
#' @export

read_portfolio_csv <- function(filepaths, combine = TRUE) {
  filepaths <- simplify_if_one_col_df(filepaths)
  stopifnot("`filepaths` must be a character vector" = typeof(filepaths) == "character")

  filepaths <- canonize_path(filepaths)

  dirs <- fs::is_dir(filepaths)
  if (any(dirs)) {
    filepaths <- c(filepaths[!dirs], fs::dir_ls(filepaths[dirs]))
  }

  pb <-
    progress::progress_bar$new(
      format = "importing :what (:current/:total) eta: :eta [:bar]",
      total = length(filepaths)
    )
  pb$tick(0)

  portfolio_dfs <- vapply(
    X = filepaths,
    FUN = function(filepath) {
      pb$tick(tokens = list(what = basename(filepath)))

      if (!is_text_file(filepath)) {
        return(list(NA))
      }

      if (!has_consistent_fields_per_line(filepath)) {
        return(list(NA))
      }

      encoding <- guess_file_encoding(filepath)
      delimiter <- guess_delimiter(filepath)
      decimal_mark <- guess_decimal_mark(filepath)
      grouping_mark <- guess_grouping_mark(filepath)
      if (any(is.na(c(encoding, delimiter, decimal_mark, grouping_mark)))) {
        return(list(NA))
      }

      locale <-
        readr::locale(
          decimal_mark = decimal_mark,
          grouping_mark = grouping_mark,
          encoding = encoding
        )

      headers <- determine_headers(filepath)
      if (length(headers) < 3) {
        return(list(NA))
      }

      col_types <-
        readr::cols(
          investor_name = readr::col_character(),
          portfolio_name = readr::col_character(),
          isin = readr::col_character(),
          market_value = readr::col_number(),
          currency = readr::col_character()
        )

      portfolio_df <-
        readr::read_delim(
          file = filepath,
          skip = 1L,
          col_names = names(headers),
          col_types = col_types,
          locale = locale,
          delim = delimiter,
          show_col_types = FALSE,
          progress = FALSE
        )

      portfolio_df <-
        dplyr::select(
          portfolio_df,
          .data$investor_name,
          .data$portfolio_name,
          .data$isin,
          .data$market_value,
          .data$currency
        )

      guessed_attrs <-
        list(
          filename = basename(filepath),
          filepath = filepath,
          has_newline_at_end = has_newline_at_end(filepath),
          encoding = encoding,
          delimiter = delimiter,
          original_headers = as.character(headers),
          num_of_cols = attr(headers, which = "num_of_cols"),
          decimal_mark = decimal_mark,
          grouping_mark = grouping_mark
        )
      attr(portfolio_df, which = "read_portfolio_csv") <- guessed_attrs

      list(portfolio_df)
    },
    FUN.VALUE = list(1L),
    USE.NAMES = TRUE
  )

  if (length(portfolio_dfs) == 1L) { return(portfolio_dfs[[1L]]) }

  if (combine) {
    return(dplyr::bind_rows(portfolio_dfs[!is.na(portfolio_dfs)], .id = "filepath"))
  }

  portfolio_dfs
}
