#' Validate a vector of filenames or filepaths
#'
#' This function validates that a vector of filenames or filepaths are
#' accessible files that: are a file, exist, have read access, and are not
#' empty. Dropbox files that are visible but not downloaded locally will be
#' empty files, and they will not pass this validation.
#'
#' @param filepaths A character vector
#'
#' @return A logical vector the same length as `filepaths`.
#' @export
is_file_accessible <- function(filepaths) {
  if (is.data.frame(filepaths) && identical(length(filepaths), 1L)) {
    filepaths <- filepaths[[1L]]
  }

  filepaths <- as.character(filepaths)
  filepaths[is.na(filepaths)] <- FALSE

  filepaths <- fs::path_abs(fs::path_expand(filepaths))

  file_is_file <- fs::is_file(filepaths)
  file_exists <- fs::file_exists(filepaths)
  file_is_readable <- is_readable_file(filepaths)
  file_not_empty <- !fs::is_file_empty(filepaths)

  unname(file_is_file & file_exists & file_is_readable & file_not_empty)
}
