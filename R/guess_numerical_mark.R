guess_numerical_mark <- function(filepaths, type = "decimal") {
  filepaths <- simplify_if_one_col_df(filepaths)
  stopifnot("`filepaths` must be a character vector" = typeof(filepaths) == "character")
  filepaths <- canonize_path(filepaths)

  vapply(
    X = filepaths,
    FUN = function(filepath) {
      if (!is_file_accessible(filepath) || !is_text_file(filepath)) {
        return(NA_character_)
      }

      encoding <- guess_file_encoding(filepath)
      delimiter <- guess_delimiter(filepath)

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
          col_names = FALSE,
          skip = 1,
          progress = FALSE,
          show_col_types = FALSE
        )

      if (ncol(char_data) < 4) {
        return(NA_character_)
      }

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
        grouping_mark <- ","
      } else if (dot_only_before_3 && !comma_only_before_3) {
        decimal_mark <- ","
        grouping_mark <- "."
      } else if (has_comma && !comma_only_before_3 && !dot_only_before_3) {
        decimal_mark <- ","
        grouping_mark <- "."
      } else if (has_dot && !dot_only_before_3 && !comma_only_before_3) {
        decimal_mark <- "."
        grouping_mark <- ","
      }

      if (nums_have_space) {
        grouping_mark <- " "
      }

      ifelse(type == "decimal", decimal_mark, grouping_mark)
    },
    FUN.VALUE = character(1L),
    USE.NAMES = FALSE
  )
}

guess_decimal_mark <- function(filepaths) {
  guess_numerical_mark(filepaths, type = "decimal")
}

guess_grouping_mark <- function(filepaths) {
  guess_numerical_mark(filepaths, type = "grouping")
}
