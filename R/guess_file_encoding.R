#' Guess the file encoding for a vector of filenames or filepaths
#'
#' This function will guess the file encoding of a vector of filenames or
#' filepaths and return the file encoding as a string. It primarily uses
#' `readr::guess_encoding()` with the `n_max = -1L` option so that all lines
#' are used to guess the encoding. If a file is inaccessible or if it is a
#' binary file, it will return `NA` for that element.
#'
#' @param filepaths A character vector
#'
#' @return A character vector the same length as `filepaths`.
#' @export
guess_file_encoding <- function(filepaths) {
  if (is.data.frame(filepaths) && identical(length(filepaths), 1L)) {
    filepaths <- filepaths[[1L]]
  }

  file_encodings <-
    vapply(
      X = filepaths,
      FUN = function(filepath) {
        if (!is_file_accessible(filepath) || is_binary_file(filepath)) {
          return(NA_character_)
        }
        readr::guess_encoding(file = filepath, n_max = -1L)$encoding[[1L]]
      },
      FUN.VALUE = character(1L),
      USE.NAMES = FALSE
    )

  has_cp850_chars <-
    vapply(
      X = filepaths,
      FUN = function(filepath) {
        if (!is_file_accessible(filepath) || is_binary_file(filepath)) {
          return(FALSE)
        }
        # ADD TEST FOR BINARY FILES
        raw_lines <-
          readr::read_lines_raw(
            file = filepath,
            n_max = -1L,
            progress = FALSE
          )
        any(
          vapply(
            X = unlist(raw_lines),
            FUN = function(raw_char) { identical(raw_char, as.raw(0x94)) },
            FUN.VALUE = logical(1L)
          )
        )
      },
      FUN.VALUE = logical(1L),
      USE.NAMES = FALSE
    )

  ifelse(has_cp850_chars, "cp850", file_encodings)
}
