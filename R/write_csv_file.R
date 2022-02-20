#' A version of readr::write_csv() where `file` falls back to `path` if needed
#'
#' @param file File or connection to write to. In readr < 1.4.0 the name of this
#'   argument is instead `path`. This wrapper helps use `file` regardless the
#'   version of readr.
#' @inheritParams readr::write_csv
#' @param ... Other arguments passed to readr::write_csv.
#' @export
write_csv_file <- function(x, file, ...) {
  if (utils::packageVersion("readr") >= "1.4.0") {
    readr::write_csv(x = x, file = file, ...)
  } else {
    readr::write_csv(x = x, path = file, ...)
  }
}
