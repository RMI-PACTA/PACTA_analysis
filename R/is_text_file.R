#' Guess if a file is a text file for a vector of filenames or filepaths
#'
#' This function will guess if a file is a text file for a vector of filenames
#' or filepaths and return `TRUE` or `FALSE` for each. It primarily uses the
#' system's `file -b --mime-type` to guess the content type, but if `file` is
#' not found on the system's path (e.g. on Windows without Rtools installed)
#' and/or if the `find` path is not executable it will fall back on using
#' `wand::get_content_type()`, which is R-only and cross platform compatible
#' but less robust.
#'
#' @param filepaths A character vector
#'
#' @return A logical vector the same length as `filepaths`.
#' @export
is_text_file <- function(filepaths) {
  if (is.data.frame(filepaths) && identical(length(filepaths), 1L)) {
    filepaths <- filepaths[[1L]]
  }

  mime_type <- function(filepath) {
    file_command <- Sys.which("file")
    if (fs::file_access(file_command, mode = "execute")) {
      system2(
        command = "file",
        args = c("-b", "--mime-type", shQuote(filepath)),
        stdout = TRUE
      )
    } else {
      wand::get_content_type(filepath)[[1L]]
    }
  }

  vapply(
    X = filepaths,
    FUN = function(filepath) {
      identical(sub("/.*$", "", mime_type(filepath)), "text")
    },
    FUN.VALUE = logical(1L),
    USE.NAMES = FALSE
  )
}
