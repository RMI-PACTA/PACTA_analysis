#' @examples
#' obj <- 1L
#' exists("obj")
#' remove_if_exists(obj)
#' exists("obj")
#'
#' exists("obj2")
#' remove_if_exists(obj2)
#' @noRd
remove_if_exists <- function(obj) {
  name <- deparse(substitute(obj))
  if (exists(name, envir = parent.frame())) {
    rm(list = name, envir = parent.frame())
  }
  invisible(name)
}
