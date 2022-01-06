#' Validate read access to files in a vector of filenames or filepaths
#'
#' This function validates that files in a vector of filenames or filepaths are
#' readable files and returns `TRUE` or `FALSE` for each one.
#'
#' @param filepaths A character vector
#'
#' @return A logical vector the same length as `filepaths`.
#'
#' @export
#'
is_readable_file <- function(filepaths) {
  vapply(
    X = filepaths,
    FUN = function(filepath) {
      !identical(
        class(
          try(suppressWarnings(readLines(filepath, n = 1L)), silent = TRUE)
        ),
        "try-error"
      )
    },
    FUN.VALUE = logical(1L),
    USE.NAMES = FALSE
  )
}
