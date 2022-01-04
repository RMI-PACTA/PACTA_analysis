#' Guess if a file is binary for a vector of filenames or filepaths
#'
#' This function will guess if a file is binary for a vector of filenames or
#' filepaths and return `TRUE` for each if so. It uses a vectorized version of
#' `file -b --mime-encoding` through `system2()` to guess. If the file is
#' inaccessible, it will return `NA` for that element.
#'
#' @param filepaths A character vector
#'
#' @return A logical vector the same length as `filepaths`.
#' @export
is_binary_file <- function(filepaths) {
  if (is.data.frame(filepaths) && identical(length(filepaths), 1L)) {
    filepaths <- filepaths[[1L]]
  }

  vapply(
    X = filepaths,
    FUN = function(filepath) {
      if (!is_file_accessible(filepath)) {
        return(NA)
      }
      identical(
        system2(
          command = "file",
          args = c("-b", "--mime-encoding", shQuote(filepath)),
          stdout = TRUE
        ),
        "binary"
      )
    },
    FUN.VALUE = logical(1L),
    USE.NAMES = FALSE
    )
}
