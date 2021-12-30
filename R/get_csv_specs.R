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

# files <- "~/Dropbox (2° Investing)/2° Investing Team/1. RESEARCH/1. Studies (projects)/PACTA . Regulator Monitoring/PACTA COP/03_Initiative_Level/04_PACTACOP_LU/04_Input_Cleaning/portfolios"
# get_csv_specs("~/Dropbox (2° Investing)/2° Investing Team/1. RESEARCH/1. Studies (projects)/PACTA . Regulator Monitoring/PACTA COP/03_Initiative_Level/04_PACTACOP_LU/04_Input_Cleaning/portfolios")
# get_csv_specs("~/Dropbox (2° Investing)/2° Investing Team/1. RESEARCH/1. Studies (projects)/PACTA . Regulator Monitoring/PACTA COP/03_Initiative_Level/05_PACTACOP_NO/04_Input_Cleaning/portfolios")
# get_csv_specs("/Users/cj2dii/Dropbox (2° Investing)/2° Investing Team/1. RESEARCH/1. Studies (projects)/PACTA . Regulator Monitoring/Bafu PACTA Monitoring 2020/05_Countrylevel/01_CH/05_Portfolio_Prep/swiss_portfolio_3")
# get_csv_specs("/Users/cj2dii/Desktop/real_tests/PA2021NO_2021-12-15/initiativeDownloads/")

# files <- c(list.files("~/Desktop/test/", full.names = TRUE), "XXX")
# get_csv_specs(c(list.files("~/Desktop/test/", full.names = TRUE), "XXX"))

get_csv_specs <- function(files) {
  if (length(files) == 1 && fs::is_dir(files)) {
    files <- file.path(files, list.files(files))
  }
  files_df <- tibble::tibble(input = files, id = seq_along(files))

  files_df$filename <- basename(files_df$input)
  files_df$extension <- tools:::file_ext(files_df$input)

  files_df$filepath <- fs::path_abs(fs::path_expand(files_df$input))

  cli::cli_div(theme = list(`.indented` = list(`margin-left` = 2), `.file` = list(color = "blue")))
  on.exit(cli::cli_end(), add = TRUE)

  files_df$file_exists <- unname(fs::file_exists(files_df$filepath))

  if (all(files_df$file_exists)) {
    cli::cli_alert_success("all files exist")
  } else if (all(!files_df$file_exists)) {
    cli::cli_alert_danger("none of the files exist")
    cli::cli_abort("the {.fun get_csv_specs} function had to stop because none of the files could be used")
  } else {
    alert_files <- files_df$filename[!files_df$file_exists]
    cli::cli({
      cli::cli_alert_danger("the following files do not exist and will not be considered further:")
      cli::cli_bullets(alert_files, class = "file indented")
    })
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
    cli::cli({
      cli::cli_alert_danger("the following files have a size of 0 and will not be considered further:")
      cli::cli_alert_info("this might mean that they are un-downloaded Dropbox files or are empty files", class = "indented")
      cli::cli_bullets(alert_files, class = "file indented")
    })
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
    cli::cli({
      cli::cli_alert_danger("the following files do not have read access and will not be considered further:")
      cli::cli_bullets(alert_files, class = "file indented")
    })
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
    cli::cli({
      cli::cli_alert_danger("the following files are binary files and will not be considered further:")
      cli::cli_bullets(alert_files, class = "file indented")
    })
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
    cli::cli({
      cli::cli_alert_danger("the following files are not CSV files and will not be considered further:")
      cli::cli_bullets(alert_files, class = "file indented")
    })
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
    cli::cli({
      cli::cli_alert_warning("the following files are not encoded in ASCII or UTF-8:")
      cli::cli_alert_info("this can be adapted to automatically by the {.fun read_portfolio_csv} function", class = "indented")
      cli::cli_bullets(alert_files, class = "file indented")
    })
  }

  files_df$num_of_lines <- guess_num_of_lines(files_df$filepath)

  files_df$last_line_has_newline <- vapply(files_df$filepath, function(x) { attr(R.utils::countLines.default(x), "lastLineHasNewline") }, logical(1))

  if (all(files_df$last_line_has_newline)) {
    cli::cli_alert_success("all files have a newline at the end")
  } else {
    alert_files <- files_df$filename[!files_df$last_line_has_newline]
    cli::cli({
      cli::cli_alert_warning("the following files do not have a newline at the end:")
      cli::cli_alert_info("this can be adapted to automatically by the {.fun read_portfolio_csv} function", class = "indented")
      cli::cli_bullets(alert_files, class = "file indented")
    })
  }

  files_df$delimiter <- guess_delimiters(files_df$filepath, files_df$file_encoding)

  if (all(files_df$delimiter == ",")) {
    cli::cli_alert_success(paste0("all files use {.strong ", cli::style_inverse(","), "} for a delimiter"))
  } else {
    alert_files <- files_df$filename[files_df$delimiter != ","]
    cli::cli({
      cli::cli_alert_warning(paste0("the following files do not use {.strong ", cli::style_inverse(","), "} for a delimiter:"))
      cli::cli_alert_info("this can be adapted to automatically by the {.fun read_portfolio_csv} function", class = "indented")
      cli::cli_bullets(alert_files, class = "file indented")
    })
  }

  files_df$num_of_columns <- guess_num_of_columns(files_df$filepath, files_df$file_encoding, files_df$delimiter)

  if (all(files_df$num_of_columns == 5L)) {
    cli::cli_alert_success(paste0("all files have {.strong 5} columns"))
  } else {
    alert_files <- files_df$filename[files_df$num_of_columns != 5L]
    cli::cli({
      cli::cli_alert_warning(paste0("the following files do not have {.strong 5} columns:"))
      cli::cli_alert_info("this can be adapted to automatically by the {.fun read_portfolio_csv} function", class = "indented")
      cli::cli_bullets(alert_files, class = "file indented")
    })
  }

  files_df$decimal_mark <- guess_decimal_marks(files_df$filepath, files_df$file_encoding, files_df$delimiter)

  if (all(files_df$decimal_mark == ".")) {
    cli::cli_alert_success(paste0("all files use {.strong ", cli::style_inverse("."), "} for a decimal mark"))
  } else {
    alert_files <- files_df$filename[files_df$decimal_mark != "."]
    cli::cli({
      cli::cli_alert_warning(paste0("the following files do not use {.strong ", cli::style_inverse("."), "} for a decimal mark"))
      cli::cli_alert_info("this can be adapted to automatically by the {.fun read_portfolio_csv} function", class = "indented")
      cli::cli_bullets(alert_files, class = "file indented")
    })
  }

  investor_name_colname <- "Investor.Name"
  portfolio_name_colname <- "Portfolio.Name"
  isin_colname <- "ISIN"
  market_value_colname <- "MarketValue"
  currency_colname <- "Currency"

  test <- purrr::map(seq_along(files_df$filepath), ~ readr::read_delim(files_df$filepath[.x], delim = files_df$delimiter[.x], progress = FALSE, show_col_types = FALSE))

  files_df$investor_name_is_string <- vapply(test, function(x) is.character(x[[investor_name_colname]]), logical(1))

  files_df$portfolio_name_is_string <- vapply(test, function(x) is.character(x[[portfolio_name_colname]]), logical(1))

  files_df$market_value_is_numeric <- vapply(test, function(x) is.numeric(x[[market_value_colname]]), logical(1))

  files_df$market_value_has_negatives <- vapply(test, function(x) any(x[[market_value_colname]] < 0), logical(1))

  files_df$market_value_has_nas <- vapply(test, function(x) any(is.na(x[[market_value_colname]])), logical(1))

  files_df$valid_iso4217c_codes <- validate_iso4217c(lapply(test, function(x) x[[currency_colname]]))

  files_df
}


guess_ <- function(filepaths, encodings) {
  vapply(
    X = seq_along(filepaths),
    FUN = function(i) {

    },
    FUN.VALUE = character(1)
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


guess_decimal_marks <- function(filepaths, encodings, delimiters) {
  vapply(
    X = seq_along(filepaths),
    FUN = function(i) {
      decimal_mark <- ifelse(delimiters[[i]] == ";", ",", ".")
      grouping_mark <- ifelse(decimal_mark == ",", ".", ",")

      cust_locale <- readr::locale(
        decimal_mark = decimal_mark,
        grouping_mark = grouping_mark,
        encoding = encodings[[i]]
      )

      char_data <-
        readr::read_delim(
          file = filepaths[[i]],
          delim = delimiters[[i]],
          locale = cust_locale,
          trim_ws = TRUE,
          col_types = readr::cols(.default = "c"),
          skip = 1,
          col_names = FALSE,
          show_col_types = FALSE,
          progress = FALSE
        )

      all_num_chars <- c(char_data[[4]])

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


