#' Check if a named object contains expected names
#'
#' It is based on:
#' <https://www.rdocumentation.org/packages/fgeo.tool/versions/1.2.5/topics/check_crucial_names>.
#'
#' @param x A named object.
#' @param expected_names String; expected names of `x`.
#'
#' @return Invisible `x`, or an error with informative message.
#' @export
#'
#' @family miscellaneous utility functions
#'
#' @examples
#' x <- c(a = 1)
#' check_crucial_names(x, "a")
#' try(check_crucial_names(x, "bad"))
#'
#' data <- data.frame(a = 1)
#' check_crucial_names(data, "a")
#' try(check_crucial_names(data, "bad"))
#'
#' # Applications for the error class "missing_names" ---------------------
#'
#' tryCatch(
#'   check_crucial_names(x, "bad"),
#'   error = function(e) class(e)
#' )
#'
#' # Wrapping in try() to allow running examples with no failure
#' try(
#'   # What's interesting is this
#'   tryCatch(
#'     check_crucial_names(x, "bad"),
#'     missing_names = function(e) {
#'       stop(
#'         "A different error message",
#'         call. = FALSE
#'       )
#'     }
#'   )
#' )
#'
#' testthat::expect_error(
#'   check_crucial_names(x, "bad"),
#'   class = "missing_names"
#' )
check_crucial_names <- function(x, expected_names) {
  stopifnot(rlang::is_named(x))
  stopifnot(is.character(expected_names))

  ok <- all(unique(expected_names) %in% names(x))
  if (!ok) {
    abort_missing_names(sort(setdiff(expected_names, names(x))))
  }

  invisible(x)
}

abort_missing_names <- function(missing_names) {
  rlang::abort(
    "missing_names",
    message = glue(
      "Must have missing names:
    {usethis::ui_field(missing_names)}"
    )
  )
}
