read_portfolio_csv <- function(filepaths) {
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
  portfolio_dfs
}
