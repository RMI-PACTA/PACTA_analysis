#' A version of [janitor::clean_names()] that also cleans dplyr groups
#'
#' With dplyr >= 0.8.99.9002 `janitor::clean_names()` seems to also clean groups
#' ([gist](https://gist.github.com/maurolepore/6558292c5003a15e246a88091c307b65)).
#'
#' @param data A data frame
#'
#' @family functions to deal with unclean names
#'
#' @seealso [janitor::clean_names()], [unclean_column_names()].
#'
#' @return A data frame
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' clean_column_names(group_by(tibble(x.x = 1), x.x))
clean_column_names <- function(data) {
  # clean_groups() is needless with dplyr >= 0.8.99.9002
  # https://gist.github.com/maurolepore/6558292c5003a15e246a88091c307b65
  clean_groups(janitor::clean_names(data))
}

#' Revert the effect of `clean_column_names()`
#'
#' @param data A data frame
#' @param unclean A data frame, commonly a version of `data` before running
#'   `janitor::clean_names(data)`.
#'
#' @seealso [janitor::clean_names()], [clean_column_names()].
#'
#' @family functions to deal with unclean names
#'
#' @export
#' @examples
#' library(dplyr)
#'
#' dirty <- tibble(x.x = 1, y = 1)
#' names(dirty)
#'
#' clean <- janitor::clean_names(dirty)
#' names(clean)
#'
#' names(unclean_column_names(clean, dirty))
#'
#' # Cases where data doesn't change because no name matches unclean names
#' # exactly -- after applying janitor::clean_names()
#'
#' unclean <- tibble(y = 1)
#' data <- tibble(y.y = 1)
#' data %>% unclean_column_names(unclean)
#'
#' unclean <- tibble(y.y = 1)
#' data <- tibble(y = 1)
#' data %>% unclean_column_names(unclean)
unclean_column_names <- function(data, unclean) {
  check_groups(data)
  dplyr::rename(data, !!!extract_unclean_column_names(data, unclean))
}

check_groups <- function(data) {
  g <- dplyr::group_vars(data)

  if (identical(length(g), 0L)) {
    return(data)
  }

  unknown <- setdiff(g, names(data))
  if (length(unknown) > 0L) {
    stop(
      "All groups must be known column names.\n",
      "Known: ", paste0(names(data), collapse = ", "), "\n",
      "Unknown: ", paste0(unknown, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(data)
}

clean_groups <- function(data) {
  g <- dplyr::group_vars(data)
  dplyr::grouped_df(data, vars = janitor::make_clean_names(g))
}

extract_unclean_column_names <- function(data, unclean) {
  names(unclean) %>%
    rlang::set_names() %>%
    purrr::map(extract_clean_names_from, data) %>%
    purrr::keep(~ length(.x) > 0L)
}

extract_clean_names_from <- function(x, data) {
  grep(anchor(janitor::make_clean_names(x)), names(data), value = TRUE)
}

anchor <- function(x) {
  paste0("^", x, "$")
}

#' Clean quosure expressions
#'
#' @param quo An object of class quosure or of class quosures.
#'
#' @family functions to deal with unclean names
#'
#' @return A quosure or quosures.
#' @export
#'
#' @examples
#' clean_quo(rlang::quo(x.x))
#'
#' clean_quo(rlang::quos(x.x, y.y))
clean_quo <- function(quo) {
  UseMethod("clean_quo")
}

#' @rdname clean_quo
#' @export
clean_quo.quosure <- function(quo) {
  unclean <- rlang::quo_squash(quo)
  clean <- janitor::make_clean_names(unclean)
  rlang::quo_set_expr(quo, rlang::sym(clean))
}

#' @rdname clean_quo
#' @export
clean_quo.quosures <- function(quo) {
  purrr::map(quo, clean_quo)
}

clean_quo.default <- function(quo) {
  stop("Cant' deal with objects of class: ", class(quo), call. = FALSE)
}

#' Convert quosure expressions to character
#'
#' @param quo A quosure or list with one quosure per list element.
#'
#' @family functions to deal with unclean names
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' quo <- rlang::quo(x)
#' quo_chr(quo)
#'
#' # This is a list of quosure as crated via `...`, not a quosures
#' quo_chr(list(quo, quo))
quo_chr <- function(quo) {
  UseMethod("quo_chr")
}

#' @rdname quo_chr
#' @export
quo_chr.list <- function(quo) {
  purrr::map_chr(quo, quo_chr.quosure)
}

#' @rdname quo_chr
#' @export
quo_chr.quosure <- function(quo) {
  rlang::as_string(rlang::quo_get_expr(quo))
}

quo_chr.default <- function(quo) {
  stop("Cant' deal with objects of class: ", class(quo), call. = FALSE)
}
