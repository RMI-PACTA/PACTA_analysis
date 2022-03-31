determine_headers <- function(filepath) {
  if (!is_text_file(filepath)) { return(NA_character_) }

  encoding <- guess_file_encoding(filepath)
  delimiter <- guess_delimiter(filepath)
  decimal_mark <- guess_decimal_mark(filepath)
  grouping_mark <- guess_grouping_mark(filepath)
  if (any(is.na(c(encoding, delimiter, decimal_mark, grouping_mark)))) {
    return(NA_character_)
  }

  locale <-
    readr::locale(
      decimal_mark = decimal_mark,
      grouping_mark = grouping_mark,
      encoding = encoding
    )

  first_line <- readr::read_delim(filepath, n_max = 1L, locale = locale, col_names = FALSE, show_col_types = FALSE, progress = FALSE)
  header_types <- vapply(X = readr::spec(first_line)$cols, FUN = function(x) sub("^collector_", "", class(x)[[1]]), FUN.VALUE = character(1), USE.NAMES = FALSE)

  if (all(header_types %in% c("character", "logical"))) {
    has_header <- TRUE
  } else {
    has_header <- FALSE
  }

  if (!has_header) {
    num_of_cols <- length(header_types)
    if (num_of_cols == 3) {
      col_names <- c("isin", "market_value", "currency")
      names(col_names) <- col_names
      not_portfolio_csv <- FALSE
    } else if (num_of_cols == 5) {
      col_names <- c("investor_name", "portfolio_name", "isin", "market_value", "currency")
      names(col_names) <- col_names
      not_portfolio_csv <- FALSE
    } else {
      not_portfolio_csv <- TRUE
    }
  } else {
    headers <- unlist(first_line, use.names = FALSE)
    num_of_cols <- length(headers)

    if (num_of_cols >= 3) {
      isin_col <- grep(pattern = "^[[:space:]]?isin[[:space:]]?$", x = headers, ignore.case = TRUE, value = TRUE)
      market_value_col <- grep(pattern = "^[[:space:]]?market[._ ]{0,1}value[[:space:]]?$", x = headers, ignore.case = TRUE, value = TRUE)
      currency_col <- grep(pattern = "^[[:space:]]?currency[[:space:]]?$", x = headers, ignore.case = TRUE, value = TRUE)

      if (num_of_cols > 3) {
        investor_name_col <- grep(pattern = "investor", x = headers, ignore.case = TRUE, value = TRUE)
        portfolio_name_col <- grep(pattern = "portfolio", x = headers, ignore.case = TRUE, value = TRUE)
      }

      if (length(isin_col) < 1 || length(market_value_col) < 1 || length(currency_col) < 1) {
        not_portfolio_csv <- TRUE
      } else {
        col_names <- c(isin_col[[1]], market_value_col[[1]], currency_col[[1]])

        if (num_of_cols > 3) {
          if (length(portfolio_name_col) > 0) {
            col_names <- c(portfolio_name_col[[1]], col_names)
          }
          if (length(investor_name_col) > 0) {
            col_names <- c(investor_name_col[[1]], col_names)
          }
        }

        not_portfolio_csv <- FALSE
      }
    } else {
      not_portfolio_csv <- TRUE
    }
  }

  if (not_portfolio_csv) { return(NA_character_) }
  if (!has_header) { return(col_names) }

  headers[is.na(headers)] <- ""

  names(headers) <- NA
  names(headers)[match(isin_col, headers)] <- "isin"
  names(headers)[match(market_value_col, headers)] <- "market_value"
  names(headers)[match(currency_col, headers)] <- "currency"
  names(headers)[match(investor_name_col, headers)] <- "investor_name"
  names(headers)[match(portfolio_name_col, headers)] <- "portfolio_name"
  names(headers) <- vctrs::vec_as_names(names(headers), repair = "unique", quiet = TRUE)

  attr(headers, which = "header_types") <- header_types
  attr(headers, which = "num_of_cols") <- num_of_cols

  headers
}
