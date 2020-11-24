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
  if (!fs::file_exists(path)) {
    rlang::abort(glue::glue("This file must exist but it doesn't:\n{path}"))
  }

  invisible(path)
}
