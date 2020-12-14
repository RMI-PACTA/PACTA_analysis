#' @examples
#' path <- tempfile()
#'
#' abort_if_file_doesnt_exist(path)
#' fs::file_create(path)
#' abort_if_file_doesnt_exist(path)
#'
#' # Cleanup
#' fs::file_delete(path)
#' @noRd
abort_if_file_doesnt_exist <- function(path) {
  if (!file.exists(path)) {
    msg <- sprintf("This file must exist but it doesn't:\ns%", path)
    stop(mdg, call. = TRUE)
  }

  invisible(path)
}
