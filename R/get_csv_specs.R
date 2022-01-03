# - [X] absolute filepath
# - [X] encoding of the filepath
# - [X] file name
# - [X] file extension
# - [X] file size
# - [X] file is not empty
# - [X] file exists?
# - [X] file is readable?
# - [X] file is writable?
# - [X] file is not binary?
# - [X] number of rows
# - [X] does it have a newline at the end
# - [X] determine best guess encoding
# - [ ] determine if best guess encoding is used and then converted to UTF-8 there are non-UTF-8 characters
# - [X] determine delimiter
# - [X] determine decimal mark
# - [ ] determine grouping mark
# - [X] determine number of columns
# - [ ] determine/guess if a header is included
# - [ ] determine in column names match expectation
# - [ ] is investor_name column all strings
# - [ ] is portfolio_name column all strings
# - [ ] is isin column all strings
# - [ ] is isin column all valid isins (Luhn)
# - [ ] is isin column all isins that are in our financial data
# - [ ] is market_value all numeric
# - [ ] is market_value all numeric after using determined decimal and grouping mark
# - [ ] is market value negative
# - [ ] is currency column all strings
# - [ ] is currency column all 3 character
# - [ ] is currency column all alpha strings
# - [ ] is currency column all uppercase
# - [x] is currency column all valid, current ISO 4217 alpha currency codes
# - [ ] is currency column all currency codes that exist in our currency exchange rate data

# get_csv_specs("~/Dropbox (2° Investing)/2° Investing Team/1. RESEARCH/1. Studies (projects)/PACTA . Regulator Monitoring/Bafu PACTA Monitoring 2020/05_Countrylevel/01_CH/05_Portfolio_Prep/swiss_portfolio_3")
# get_csv_specs("~/Dropbox (2° Investing)/2° Investing Team/1. RESEARCH/1. Studies (projects)/PACTA . Regulator Monitoring/Bafu PACTA Monitoring 2020/05_Countrylevel/02_Liechtenstein/08_input_cleaning/Liechtenstein_Portfolios_original")
# get_csv_specs("~/Dropbox (2° Investing)/2° Investing Team/1. RESEARCH/1. Studies (projects)/PACTA . Regulator Monitoring/PACTA COP/03_Initiative_Level/04_PACTACOP_LU/04_Input_Cleaning/portfolios")
# get_csv_specs("~/Dropbox (2° Investing)/2° Investing Team/1. RESEARCH/1. Studies (projects)/PACTA . Regulator Monitoring/PACTA COP/03_Initiative_Level/05_PACTACOP_NO/04_Input_Cleaning/portfolios")
# get_csv_specs("~/Dropbox (2° Investing)/2° Investing Team/1. RESEARCH/1. Studies (projects)/PACTA . Regulator Monitoring/PACTA COP/03_Initiative_Level/06_PACTACOP_CO/042_Input_Cleaning/portfolios")
# get_csv_specs("~/Dropbox (2° Investing)/2° Investing Team/1. RESEARCH/1. Studies (projects)/PACTA . Regulator Monitoring/PACTA COP/03_Initiative_Level/06_PACTACOP_PE/04_Input_Cleaning/04. Data/portfolios")

# get_csv_specs("~/Desktop/real_tests/PA2021NO_2021-12-15/initiativeDownloads/")
# get_csv_specs("~/Desktop/real_tests/PA2021CO_2021-12-15/initiativeDownloads/")

# files <- c(list.files("~/Desktop/test/", full.names = TRUE), "XXX")
# get_csv_specs(files = c(list.files("~/Desktop/test/", full.names = TRUE), "XXX"), expected_colnames = c("investor_name", "portfolio_name", "isin", "market_value", "currency"))

get_csv_specs <- function(files, expected_colnames = c("Investor.Name", "Portfolio.Name", "ISIN", "MarketValue", "Currency")) {
  alert_by_type <- function(type, ...) {
    switch(
      type,
      info = cli::cli_alert_info(...),
      warning = cli::cli_alert_warning(...),
      danger = cli::cli_alert_danger(...),
      success = cli::cli_alert_success(...),
    )
  }

  report_alert_files <- function(msg, bullets, type = "info", info = NULL) {
    cli::cli_div(theme = list(`.indented` = list(`margin-left` = 2), `.file` = list(color = "blue")))
    on.exit(cli::cli_end(), add = TRUE)

    cli::cli({
      alert_by_type(type, msg)
      if (!is.null(info)) {
        alert_by_type("info", info, class = "indented")
      }
      if (length(bullets) > 10L) {
        abbreviated <- c(bullets[1:10], paste0("… and {.strong ", length(bullets) - 10, " more}"))
        cli::cli_bullets(abbreviated, class = "file indented")
      } else {
        cli::cli_bullets(bullets, class = "file indented")
      }
    })
  }

  if (length(files) == 1 && fs::is_dir(files)) {
    files <- file.path(files, list.files(files))
  }
  files_df <- tibble::tibble(input = files, id = seq_along(files))

  files_df$filename <- basename(files_df$input)
  files_df$extension <- tools:::file_ext(files_df$input)

  files_df$filepath <- fs::path_abs(fs::path_expand(files_df$input))

  files_df$file_exists <- unname(fs::file_exists(files_df$filepath))

  if (all(files_df$file_exists)) {
    cli::cli_alert_success("all files exist")
  } else if (all(!files_df$file_exists)) {
    cli::cli_alert_danger("none of the files exist")
    cli::cli_abort("the {.fun get_csv_specs} function had to stop because none of the files could be used")
  } else {
    alert_files <- files_df$filename[!files_df$file_exists]
    report_alert_files("the following files do not exist and will not be considered further:", alert_files, type = "danger")
    files_df <- files_df[files_df$file_exists, ]
  }

  files_df$file_size <- unname(fs::file_size(files_df$filepath))

  if (all(files_df$file_size > 0)) {
    cli::cli_alert_success("all files have a size > 0")
  } else if (all(files_df$file_size == 0)) {
    cli::cli({
      cli::cli_alert_danger("all of the files have a size of 0")
      cli::cli_alert_info("this might mean that they are un-downloaded Dropbox files or are empty files", class = "indented")
    })
    cli::cli_abort("the {.fun get_csv_specs} function had to stop because none of the files could be used")
  } else {
    alert_files <- files_df$filename[files_df$file_size == 0]
    report_alert_files("the following files have a size of 0 and will not be considered further:", alert_files, type = "danger", info = "this might mean that they are un-downloaded Dropbox files or are empty files")
    files_df <- files_df[files_df$file_size > 0, ]
  }


  files_df$file_read_access <- unname(fs::file_access(files_df$filepath, mode = "read"))

  if (all(files_df$file_read_access)) {
    cli::cli_alert_success("all files have read access")
  } else if (all(!files_df$file_read_access)) {
    cli::cli_alert_danger("all of the files do not have read access")
    cli::cli_abort("the {.fun get_csv_specs} function had to stop because none of the files could be used")
  } else {
    alert_files <- files_df$filename[!files_df$file_read_access]
    report_alert_files("the following files do not have read access and will not be considered further:", alert_files, type = "danger")
    files_df <- files_df[files_df$file_read_access, ]
  }


  files_df$file_write_access <- unname(fs::file_access(files_df$filepath, mode = "write"))

  files_df$mime_encoding <- vapply(files_df$filepath, function(x) system2("file", args = c("-b", "--mime-encoding", shQuote(x)), stdout = TRUE), character(1))

  if (all(files_df$mime_encoding != "binary")) {
    cli::cli_alert_success("all files are not binary files")
  } else if (all(files_df$mime_encoding == "binary")) {
    cli::cli_alert_danger("all of the files are binary files")
    cli::cli_abort("the {.fun get_csv_specs} function had to stop because none of the files could be used")
  } else {
    alert_files <- files_df$filename[files_df$mime_encoding == "binary"]
    report_alert_files("the following files are binary files and will not be considered further:", alert_files, type = "danger")
    files_df <- files_df[files_df$mime_encoding != "binary", ]
  }


  files_df$content_type <- guess_content_types(files_df$filepath)

  if (all(grepl("/csv$|/comma-separated-values$", files_df$content_type))) {
    cli::cli_alert_success("all files are CSV files")
  } else if (all(!grepl("/csv$|/comma-separated-values$", files_df$content_type))) {
    cli::cli_alert_danger("all of the files are not CSV files")
    cli::cli_abort("the {.fun get_csv_specs} function had to stop because none of the files could be used")
  } else {
    alert_files <- files_df$filename[!grepl("/csv$|/comma-separated-values$", files_df$content_type)]
    report_alert_files("the following files are not CSV files and will not be considered further:", alert_files, type = "danger")
    files_df <- files_df[grepl("/csv$|/comma-separated-values$", files_df$content_type), ]
  }

  files_df$filepath_is_ascii <- stringi::stri_enc_isascii(files_df$filepath)

  detected_filepath_encoding_tbls <- stringi::stri_enc_detect(files_df$filepath)
  files_df$filepath_encoding <- vapply(detected_filepath_encoding_tbls, function(x) { x$Encoding[[1]] }, character(1))

  files_df$filepath_declared_encoding <- Encoding(files_df$filepath)

  # if (all(files_df$filepath_is_ascii)) {
  #   cli::cli_alert_success("all filepaths are in ASCII")
  # } else {
  #   # cli::cli_alert_warning("the following filepaths are not in ASCII")
  #   # cli::cli_bullets(setNames(files_df$filepath[!files_df$filepath_is_ascii], " "))
  #   cli::cli_text("the following filepaths are not in ASCII: {.file {files_df$filepath[!files_df$filepath_is_ascii]}}")
  # }

  # if (any(files_df$filepath_encoding == "UTF-8")) {
  #   # cli::cli_alert_warning("the following filepaths are in UTF-8, which can cause trouble, especially on Windows")
  #   cli::cli_text("the following filepaths are in UTF-8, which can cause trouble, especially on Windows: {.file {files_df$filepath[!files_df$filepath_is_ascii]}}")
  # }


  files_df$file_encoding <- guess_encodings(files_df$filepath)

  if (all(files_df$file_encoding %in% c("ASCII", "UTF-8"))) {
    cli::cli_alert_success("all files are encoded in ASCII or UTF-8")
  } else {
    alert_files <- files_df$filename[!files_df$file_encoding %in% c("ASCII", "UTF-8")]
    report_alert_files("the following files are not encoded in ASCII or UTF-8:", alert_files, type = "warning", info = "this can be adapted to automatically by the {.fun read_portfolio_csv} function")
  }

  files_df$num_of_lines <- guess_num_of_lines(files_df$filepath)

  files_df$last_line_has_newline <- vapply(files_df$filepath, function(x) { attr(R.utils::countLines.default(x), "lastLineHasNewline") }, logical(1))

  if (all(files_df$last_line_has_newline)) {
    cli::cli_alert_success("all files have a newline at the end")
  } else {
    alert_files <- files_df$filename[!files_df$last_line_has_newline]
    report_alert_files("the following files do not have a newline at the end:", alert_files, type = "warning", info = "this can be adapted to automatically by the {.fun read_portfolio_csv} function")
  }

  files_df$delimiter <- guess_delimiters(files_df$filepath, files_df$file_encoding)

  if (all(files_df$delimiter == ",")) {
    cli::cli_alert_success(paste0("all files use {.strong ", cli::style_inverse(","), "} for a delimiter"))
  } else {
    alert_files <- files_df$filename[files_df$delimiter != ","]
    report_alert_files(paste0("the following files do not use {.strong ", cli::style_inverse(","), "} for a delimiter:"), alert_files, type = "warning", info = "this can be adapted to automatically by the {.fun read_portfolio_csv} function")
  }

  files_df$read_without_error <- validate_read_without_error(files_df$filepath, files_df$file_encoding, files_df$delimiter)

  if (all(files_df$read_without_error == TRUE)) {
    cli::cli_alert_success(paste0("all files can be read without error"))
  } else if (any(files_df$read_without_error == FALSE)) {
    alert_files <- files_df$filename[isFALSE(files_df$read_without_error)]
    report_alert_files("the following files can not be read without error and will not be considered further:", alert_files, type = "danger")
    files_df <- files_df[files_df$read_without_error == TRUE, ]
  }


  files_df$num_of_columns <- guess_num_of_columns(files_df$filepath, files_df$file_encoding, files_df$delimiter)

  if (all(files_df$num_of_columns == 5L)) {
    cli::cli_alert_success(paste0("all files have {.strong 5} columns"))
  } else if (any(files_df$num_of_columns > 5L)) {
    alert_files <- files_df$filename[files_df$num_of_columns > 5L]
    report_alert_files("the following files have more than {.strong 5} columns:", alert_files, type = "warning", info = "this can usually be adapted to automatically by the {.fun read_portfolio_csv} function")
  } else if (any(files_df$num_of_columns < 4L)) {
    alert_files <- files_df$filename[files_df$num_of_columns < 4L]
    report_alert_files("the following files have less than {.strong 4} columns and will not be considered further:", alert_files, type = "danger")
    files_df <- files_df[files_df$num_of_columns >= 4L, ]
  }



  files_df$decimal_mark <- guess_decimal_marks(files_df$filepath, files_df$file_encoding, files_df$delimiter)

  if (all(files_df$decimal_mark == ".")) {
    cli::cli_alert_success(paste0("all files use {.strong ", cli::style_inverse("."), "} for a decimal mark"))
  } else {
    alert_files <- files_df$filename[files_df$decimal_mark != "."]
    report_alert_files(paste0("the following files do not use {.strong ", cli::style_inverse("."), "} for a decimal mark"), alert_files, type = "warning", info = "this can be adapted to automatically by the {.fun read_portfolio_csv} function")
  }

  files_df$grouping_mark <- guess_grouping_marks(filepaths = files_df$filepath, encodings = files_df$file_encoding, delimiters = files_df$delimiter)

  if (all(files_df$grouping_mark == ",")) {
    cli::cli_alert_success(paste0("all files use {.strong ", cli::style_inverse(","), "} for a grouping mark"))
  } else {
    alert_files <- files_df$filename[files_df$grouping_mark != ","]
    report_alert_files(paste0("the following files do not use {.strong ", cli::style_inverse(","), "} for a grouping mark"), alert_files, type = "warning", info = "this can be adapted to automatically by the {.fun read_portfolio_csv} function")
  }

  files_df$tokenizer <- get_tokenizers(files_df$filepath, files_df$file_encoding, files_df$delimiter)

  files_df$fields_per_line <- get_fields_per_line(files_df$filepath, files_df$tokenizer)
  files_df$has_consistent_fields_per_line <- vapply(X = files_df$fields_per_line, FUN = function(x) all(x == x[1]), FUN.VALUE = logical(1))

  if (all(files_df$has_consistent_fields_per_line == TRUE)) {
    cli::cli_alert_success(paste0("all files have a consistent number of fields per line"))
  } else if (any(files_df$has_consistent_fields_per_line == FALSE)) {
    alert_files <- files_df$filename[files_df$has_consistent_fields_per_line == FALSE]
    report_alert_files("the following files do not have a consistent number of fields per line and will not be considered further:", alert_files, type = "danger")
    files_df <- files_df[files_df$has_consistent_fields_per_line == TRUE, ]
  }

  files_df$column_names <- get_column_names(files_df$filepath, files_df$file_encoding, files_df$delimiter)

  files_df$has_expected_colnames <- vapply(X = files_df$column_names, FUN = function(x) isTRUE(all.equal(target = expected_colnames, current = x)), FUN.VALUE = logical(1))

  if (all(files_df$has_expected_colnames == TRUE)) {
    cli::cli_alert_success(paste0("all files have the expected column names"))
  } else if (all(files_df$has_expected_colnames == FALSE)) {
    cli::cli_alert_warning("none of the files have the expected column names")
  } else if (any(files_df$has_expected_colnames == FALSE)) {
    alert_files <- files_df$filename[files_df$has_expected_colnames == FALSE]
    report_alert_files("the following files do not have the expected column names:", alert_files, type = "warning")
  }

  files_df$readr_locale <- get_locales(encodings = files_df$file_encoding, decimal_marks = files_df$decimal_mark, grouping_marks = files_df$grouping_mark)

  investor_name_colname <- expected_colnames[[1]]
  portfolio_name_colname <- expected_colnames[[2]]
  isin_colname <- expected_colnames[[3]]
  market_value_colname <- expected_colnames[[4]]
  currency_colname <- expected_colnames[[5]]
  test <- purrr::map(seq_along(files_df$filepath), ~ suppressWarnings(suppressMessages(readr::read_delim(files_df$filepath[.x], delim = files_df$delimiter[.x], locale = files_df$readr_locale[.x][[1]], progress = FALSE, show_col_types = FALSE))))

  files_df$investor_name_is_string <- vapply(test, function(x) is.character(x[[investor_name_colname]]), logical(1))

  files_df$portfolio_name_is_string <- vapply(test, function(x) is.character(x[[portfolio_name_colname]]), logical(1))

  files_df$market_value_is_numeric <- vapply(test, function(x) is.numeric(x[[market_value_colname]]), logical(1))

  files_df$market_value_has_negatives <- vapply(test, function(x) any(x[[market_value_colname]] < 0), logical(1))

  files_df$market_value_has_nas <- vapply(test, function(x) any(is.na(x[[market_value_colname]])), logical(1))

  files_df$valid_iso4217c_codes <- validate_iso4217c(lapply(test, function(x) x[[currency_colname]]))

  files_df$valid_isins <- validate_isins_list(lapply(test, function(x) x[[isin_colname]]))

  files_df$has_invalid_isins <- vapply(files_df$valid_isins, function(x) any(x == FALSE), FUN.VALUE = logical(1), USE.NAMES = FALSE)

  if (all(files_df$has_invalid_isins == FALSE)) {
    cli::cli_alert_success(paste0("all files have only valid ISINs"))
  } else if (any(files_df$has_invalid_isins == TRUE)) {
    alert_files <- files_df$filename[files_df$has_invalid_isins == TRUE]
    report_alert_files("the following files have some invalid ISINs:", alert_files, type = "warning")
  }


  invisible(files_df)
}


guess_ <- function(filepaths, encodings) {
  vapply(
    X = seq_along(filepaths),
    FUN = function(i) {

    },
    FUN.VALUE = character(1)
  )
}


check_column_names <- function(filepaths, encodings, delimiters) {
  vapply(
    X = seq_along(filepaths),
    FUN = function(i) {
      readr::read_delim(file = filepaths[[i]], locale = readr::locale(encoding = encodings[[i]]), delim = delimiters[[i]], n_max = 1L, col_names = FALSE, show_col_types = FALSE, progress = FALSE)
    },
    FUN.VALUE = integer(1)
  )
}

get_column_names <- function(filepaths, encodings, delimiters) {
  vapply(
    X = seq_along(filepaths),
    FUN = function(i) {
      list(
        names(
          suppressMessages(
            readr::read_delim(
              file = filepaths[[i]],
              delim = delimiters[[i]],
              locale = readr::locale(encoding = encodings[[i]]),
              n_max = 1L,
              trim_ws = TRUE,
              col_types = readr::cols(.default = "c"),
              show_col_types = FALSE,
              progress = FALSE
            )
          )
        )
      )
    },
    FUN.VALUE = list(1)
  )
}


get_fields_per_line <- function(filepaths, tokenizers) {
  vapply(
    X = seq_along(filepaths),
    FUN = function(i) {
      list(
        readr::count_fields(
          file = filepaths[[i]],
          tokenizer = tokenizers[[i]]
        )
      )
    },
    FUN.VALUE = list(1)
  )
}


get_locales <- function(encodings, decimal_marks, grouping_marks) {
  vapply(
    X = seq_along(encodings),
    FUN = function(i) {
      list(
        readr::locale(
          encoding = encodings[[i]],
          decimal_mark = decimal_marks[[i]],
          grouping_mark = grouping_marks[[i]]
        )
      )
    },
    FUN.VALUE = list(1)
  )
}


get_tokenizers <- function(filepaths, encodings, delimiters) {
  vapply(
    X = seq_along(filepaths),
    FUN = function(i) {
      list(
        readr::tokenizer_delim(
          delim = delimiters[[i]],
          quote = "\"",
          na = "NA",
          quoted_na = TRUE,
          comment = "",
          trim_ws = TRUE,
          escape_double = TRUE,
          escape_backslash = FALSE,
          skip_empty_rows = TRUE
        )
      )
    },
    FUN.VALUE = list(1)
  )
}


guess_content_types <- function(filepaths) {
  vapply(
    X = filepaths,
    FUN = function(x) {
      wand::guess_content_type(x)[[1]]
    },
    FUN.VALUE = character(1))
}


guess_encodings <- function(filepaths) {
  file_encodings <-
    vapply(X = filepaths,
           FUN = function(x) {
             readr::guess_encoding(file = x, n_max = -1, threshold = 0)$encoding[[1]]
           },
           FUN.VALUE = character(1),
           USE.NAMES = FALSE)

  raw_lines <- lapply(X = filepaths, FUN = readr::read_lines_raw)
  has_cp850_chars <-
    vapply(
      X = raw_lines,
      FUN = function(x) {
        any(
          vapply(
            X = x,
            FUN = function(y) {
              any(y == as.raw(0x94))
              },
            FUN.VALUE = logical(1)
          )
        )
      },
      FUN.VALUE = logical(1)
    )

  ifelse(has_cp850_chars, "cp850", file_encodings)
}


guess_decimal_mark <- function(filepath, encoding, delimiter) {
  decimal_mark <- ifelse(delimiter == ";", ",", ".")
  grouping_mark <- ifelse(decimal_mark == ",", ".", ",")

  cust_locale <-
    readr::locale(
      decimal_mark = decimal_mark,
      grouping_mark = grouping_mark,
      encoding = encoding
    )

  char_data <-
    readr::read_delim(
      file = filepath,
      delim = delimiter,
      locale = cust_locale,
      trim_ws = TRUE,
      col_types = readr::cols(.default = "c"),
      col_names = TRUE,
      progress = FALSE
    )

  all_num_chars <- char_data[[4]]

  grp_mrk_com_regex <- "^((?![,]).)*$|[,][[:digit:]]{3}[^[:digit:]]|[,][[:digit:]]{3}$"
  grp_mrk_dot_regex <- "^((?![.]).)*$|[.][[:digit:]]{3}[^[:digit:]]|[.][[:digit:]]{3}$"

  has_comma <- any(grepl("[,]", all_num_chars))
  has_dot <- any(grepl("[.]", all_num_chars))

  comma_only_before_3 <- all(has_comma, grepl(grp_mrk_com_regex, all_num_chars, perl = TRUE))
  dot_only_before_3 <- all(has_dot, grepl(grp_mrk_dot_regex, all_num_chars, perl = TRUE))

  nums_have_space <- any(grepl(" ", char_data[[4]]))

  if (comma_only_before_3 && !dot_only_before_3) {
    decimal_mark <- "."
  } else if (dot_only_before_3 && !comma_only_before_3) {
    decimal_mark <- ","
  } else if (has_comma && !comma_only_before_3 && !dot_only_before_3) {
    decimal_mark <- ","
  } else if (has_dot && !dot_only_before_3 && !comma_only_before_3) {
    decimal_mark <- "."
  }

  decimal_mark
}


guess_decimal_marks <- function(filepaths, encodings, delimiters) {
  vapply(
    X = seq_along(filepaths),
    FUN = function(i) {
      guess_decimal_mark(
        filepath = filepaths[[i]],
        encoding = encodings[[i]],
        delimiter = delimiters[[i]]
      )
    },
    FUN.VALUE = character(1)
  )
}


guess_delimiters <- function(filepaths, encodings) {
  vapply(
    X = seq_along(filepaths),
    FUN = function(i) {
      line1 <- readr::read_lines(file = filepaths[[i]], locale = readr::locale(encoding = encodings[[i]]), n_max = 1, progress = FALSE)
      commas <- stringr::str_count(line1, ",")
      semicolons <- stringr::str_count(line1, ";")
      delim <- ifelse(commas > semicolons, ",", ";")
    },
    FUN.VALUE = character(1)
  )
}


guess_grouping_mark <- function(filepath, encoding, delimiter) {
  decimal_mark <- ifelse(delimiter == ";", ",", ".")
  grouping_mark <- ifelse(decimal_mark == ",", ".", ",")

  cust_locale <-
    readr::locale(
      decimal_mark = decimal_mark,
      grouping_mark = grouping_mark,
      encoding = encoding
    )

  char_data <-
    readr::read_delim(
      file = filepath,
      delim = delimiter,
      locale = cust_locale,
      trim_ws = TRUE,
      col_types = readr::cols(.default = "c"),
      col_names = TRUE,
      progress = FALSE
    )

  all_num_chars <- char_data[[4]]

  grp_mrk_com_regex <- "^((?![,]).)*$|[,][[:digit:]]{3}[^[:digit:]]|[,][[:digit:]]{3}$"
  grp_mrk_dot_regex <- "^((?![.]).)*$|[.][[:digit:]]{3}[^[:digit:]]|[.][[:digit:]]{3}$"

  has_comma <- any(grepl("[,]", all_num_chars))
  has_dot <- any(grepl("[.]", all_num_chars))

  comma_only_before_3 <- all(has_comma, grepl(grp_mrk_com_regex, all_num_chars, perl = TRUE))
  dot_only_before_3 <- all(has_dot, grepl(grp_mrk_dot_regex, all_num_chars, perl = TRUE))

  nums_have_space <- any(grepl(" ", char_data[[4]]))

  if (comma_only_before_3 && !dot_only_before_3) {
    grouping_mark <- ","
    decimal_mark <- "."
  } else if (dot_only_before_3 && !comma_only_before_3) {
    grouping_mark <- "."
    decimal_mark <- ","
  } else if (has_comma && !comma_only_before_3 && !dot_only_before_3) {
    grouping_mark <- "."
    decimal_mark <- ","
  } else if (has_dot && !dot_only_before_3 && !comma_only_before_3) {
    grouping_mark <- ","
    decimal_mark <- "."
  }

  if (nums_have_space) { grouping_mark <- " " }

  grouping_mark
}


guess_grouping_marks <- function(filepaths, encodings, delimiters) {
  vapply(
    X = seq_along(filepaths),
    FUN = function(i) {
      guess_grouping_mark(
        filepath = filepaths[[i]],
        encoding = encodings[[i]],
        delimiter = delimiters[[i]]
      )
    },
    FUN.VALUE = character(1)
  )
}


guess_num_of_columns <- function(filepaths, encodings, delimiters) {
  vapply(
    X = seq_along(filepaths),
    FUN = function(i) {
      ncol(readr::read_delim(file = filepaths[[i]], locale = readr::locale(encoding = encodings[[i]]), delim = delimiters[[i]], n_max = 1L, col_names = FALSE, show_col_types = FALSE, progress = FALSE))
    },
    FUN.VALUE = integer(1)
  )
}


guess_num_of_lines <- function(filepaths) {
  vapply(
    X = filepaths,
    FUN = function(filepath) {
      length(readr::read_lines(filepath, n_max = -1L, lazy = TRUE, progress = FALSE))
    },
    FUN.VALUE = integer(1)
  )
}


validate_isins <- function(isins) {
  is_luhn <- function(x) {
    digits <- suppressWarnings(as.numeric(rev(unlist(strsplit(x, "")))))
    odd <- seq_along(digits) %% 2 == 1
    s1 <- sum(digits[odd])
    s2 <- digits[!odd] * 2
    s2 <- sum(s2 %% 10 + s2 %/% 10)
    sum(s1, s2) %% 10 == 0
  }

  isins <- toupper(isins)
  isins <- gsub(pattern = "[[:blank:]]", replacement = "", isins)
  valid_struct <- grepl("^[[:upper:]]{2}[[:alnum:]]{9}[[:digit:]]$", isins)

  valid_luhn <-
    vapply(
      X = isins,
      FUN = function(x) {
        x <- stringi::stri_replace_all_fixed(x, LETTERS, seq_along(LETTERS) + 9L,
                                             vectorize_all = FALSE)
        out <- vapply(x, is_luhn, FUN.VALUE = logical(1L), USE.NAMES = FALSE)
        out[is.na(out)] <- FALSE
        out
      },
      FUN.VALUE = logical(1),
      USE.NAMES = FALSE
    )

  valid_struct & valid_luhn
}


validate_isins_list <- function(isins_list) {
  if (class(isins_list) != "list") isins_list <- list(isins_list)
  lapply(X = isins_list, FUN = validate_isins)
}


validate_iso4217c <- function(codes_list) {
  if (!is.list(codes_list)) codes_list <- list(codes_list)
  vapply(
    X = codes_list,
    FUN = function(codes) {
      all(toupper(unique(codes)) %in% countrycode::codelist$iso4217c)
    },
    FUN.VALUE = logical(1)
  )
}


validate_read_without_error <- function(filepaths, encodings, delimiters) {
  vapply(
    X = seq_along(filepaths),
    FUN = function(i) {
      out <- tryCatch(
        suppressWarnings(suppressMessages(
          readr::read_delim(
            file = filepaths[[i]],
            delim = delimiters[[i]],
            locale = readr::locale(encoding = encodings[[i]]),
            progress = FALSE,
            show_col_types = FALSE
          )
        ))
      )
      !any(class(out) == "error")
    },
    FUN.VALUE = logical(1)
  )
}
