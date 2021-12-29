read_portfolio_csv <- function(filename, col_names = NULL, specs_only = FALSE) {
  stopifnot(require(readr))
  stopifnot(require(stringr))

  if (is.null(col_names)) {
    col_names <- c("investor_name", "portfolio_name", "isin", "market_value", "currency")
  }

  # figure out encoding --------------------------------------------------------

  encoding <- guess_encoding(filename, n_max = -1, threshold = 0)$encoding[1]
  has_cp850_chars <- any(unlist(read_lines_raw(filename)) == as.raw(0x94))
  encoding <- ifelse(has_cp850_chars, "cp850", encoding)

  # figure out delimiter -------------------------------------------------------

  line1 <- read_lines(filename, locale = locale(encoding = encoding), n_max = 1)

  commas <- stringr::str_count(line1, ",")
  semicolons <- stringr::str_count(line1, ";")
  delim <- ifelse(commas > semicolons, ",", ";")

  # test that delimiter matches 6 columns --------------------------------------

  num_of_columns <- ncol(read_delim(file = filename, delim = delim, n_max = 1, col_names = FALSE, col_types = cols()))
  if (num_of_columns != length(col_names)) {
    warning(paste0("number of columns is not ", length(col_names), "\n\n"))
  }

  # set basic options ----------------------------------------------------------
  decimal_mark <- ifelse(delim == ";", ",", ".")
  grouping_mark <- ifelse(decimal_mark == ",", ".", ",")

  cust_locale <- locale(decimal_mark = decimal_mark,
                        grouping_mark = grouping_mark,
                        encoding = encoding)

  # figure out odd decimal and grouping mark combos ----------------------------

  char_data <- read_delim(file = filename, delim = delim, locale = cust_locale,
                          trim_ws = TRUE, col_types = cols(.default = "c"),
                          col_names = col_names, skip = 1)

  all_num_chars <- c(char_data[[col_names[[4]]]])

  grp_mrk_com_regex <- "^((?![,]).)*$|[,][[:digit:]]{3}[^[:digit:]]|[,][[:digit:]]{3}$"
  grp_mrk_dot_regex <- "^((?![.]).)*$|[.][[:digit:]]{3}[^[:digit:]]|[.][[:digit:]]{3}$"

  has_comma <- any(grepl("[,]", all_num_chars))
  has_dot <- any(grepl("[.]", all_num_chars))

  comma_only_before_3 <- all(has_comma, grepl(grp_mrk_com_regex, all_num_chars, perl = TRUE))
  dot_only_before_3 <- all(has_dot, grepl(grp_mrk_dot_regex, all_num_chars, perl = TRUE))

  nums_have_space <- any(grepl(" ", char_data[[col_names[[4]]]]))

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

  cust_locale <- locale(decimal_mark = decimal_mark,
                        grouping_mark = grouping_mark,
                        encoding = encoding)

  # if specs_only return only the specs -----------------------------------------
  if (specs_only) {
    return(tibble(
      filename = filename,
      exists = file.exists(filename),
      num_of_columns = num_of_columns,
      file_encoding = encoding,
      delimiter = delim,
      decimal_mark = decimal_mark,
      grouping_mark = grouping_mark
    ))
  }

  # read in data ---------------------------------------------------------------

  data <- read_delim(file = filename, delim = delim, locale = cust_locale,
                     na = c("", "-", "NA"), trim_ws = TRUE,
                     col_types = "cccnc", col_names = col_names, skip = 1)

  # add attriubtes to save options ---------------------------------------------

  attr(data, "file_encoding") <- encoding
  attr(data, "delimiter") <- delim
  attr(data, "decimal_mark") <- decimal_mark
  attr(data, "grouping_mark") <- grouping_mark
  attr(data, "filename") <- filename

  return(data)
}
