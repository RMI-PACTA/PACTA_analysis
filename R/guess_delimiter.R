#' Guess the delimiter of a delimited file for a vector of filenames or
#' filepaths
#'
#' This function will guess the delimiter of a delimited file for a vector of
#' filenames or filepaths and return the delimiter as a string. It primarily
#' uses `vroom:::guess_delim()` to guess, and defaults to "," if that fails. If
#' the file is inaccessible or binary, it will return `NA` for that element.  If
#' you pass anything that is not a character vector or a single column
#' `data.frame` to the `filepaths` argument, this function will give an error.
#'
#' @param filepaths A character vector
#'
#' @return A character vector the same length as `filepaths`.
#'
#' @export
#'
guess_delimiter <- function(filepaths) {
  filepaths <- simplify_if_one_col_df(filepaths)
  stopifnot("`filepaths` must be a character vector" = typeof(filepaths) == "character")
  filepaths <- canonize_path(filepaths)

  vapply(
    X = filepaths,
    FUN = function(filepath) {
      if (!is_file_accessible(filepath) || !is_text_file(filepath)) {
        return(NA_character_)
      }
      lines <-
        readr::read_lines(
          file = filepath,
          locale = readr::locale(encoding = guess_file_encoding(filepath)),
          n_max = -1L,
          progress = FALSE
        )
      tryCatch(
        vroom:::guess_delim(lines, delims = c(",", "\t", "|", ":", ";")),
        error = function(e) ","
      )
    },
    FUN.VALUE = character(1L),
    USE.NAMES = FALSE
  )
}
