#' Guess if a file is a text file for a vector of filenames or filepaths
#'
#' This function will guess if a file is a text file for a vector of filenames
#' or filepaths and return `TRUE` or `FALSE` for each. It guesses that a file is
#' text if it doesn't find any nul bytes in the first 2048 bytes of the file.
#' This is an imperfect guess, but it is very likely that a binary/non-text file
#' will have a nul byte near the beginning of the file. This might guess that a
#' file that is not intended to be read in as text is a text file, but at least
#' you will likely be able to read in the file as text without error. If the
#' file is inaccessible, either because it is empty, you don't have permission
#' to read it, it's a directory, or it doesn't exist, this function will return
#' `FALSE`. If you pass anything that is not a character vector or a single
#' column data.frame to the `filepaths` argument, this function will give an
#' error.
#'
#' @param filepaths A character vector
#'
#' @return A logical vector the same length as `filepaths`.
#' @export
is_text_file <- function(filepaths) {
  filepaths <- simplify_if_one_col_df(filepaths)
  stopifnot("`filepaths` must be a character vector" = typeof(filepaths) == "character")
  filepaths <- canonize_path(filepaths)

  vapply(
    X = filepaths,
    FUN = function(filepath) {
      if (!is_file_accessible(filepath)) {
        return(FALSE)
      }
      !has_binary_null(filepath)
    },
    FUN.VALUE = logical(1L),
    USE.NAMES = FALSE
  )
}
