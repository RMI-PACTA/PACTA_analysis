#' Guess the MIME type of a file for a vector of filenames or filepaths
#'
#' This function will guess the MIME type of a file for a vector of filenames
#' or filepaths and return the MIME type as a string. It uses a vectorized
#' version of `wand::guess_content_type()` to guess, and defaults to `NA` if it
#' cannot determine the MIME type. If the file is inaccessible, it will return
#' `NA` for that element.
#'
#' @param filepaths A character vector
#'
#' @return A character vector the same length as `filepaths`.
#' @export
guess_file_mime_type <- function(filepaths) {
  if (is.data.frame(filepaths) && identical(length(filepaths), 1L)) {
    filepaths <- filepaths[[1L]]
  }

  vapply(
    X = filepaths,
    FUN = function(filepath) {
      if (!is_file_accessible(filepath)) {
        return(NA_character_)
      }
      out <- wand::guess_content_type(filepath)[[1L]]
      if (identical(out, "???")) {
        return(NA_character_)
      }
      out
    },
    FUN.VALUE = character(1L),
    USE.NAMES = FALSE
  )
}
