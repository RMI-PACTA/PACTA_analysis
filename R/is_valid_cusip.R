#' Validate a vector of CUSIPs
#'
#' This function validates that a vector of CUSIPs are valid CUSIP codes.
#'
#' @param cusips A character or numeric vector (automatically collapses a single
#'   column data.frame to a vector)
#'
#' @return A logical vector the same length as `cusips`.
#'
#' @export

is_valid_cusip <- function(cusips) {
  valid_cusip <- function(x) {
    if (nchar(x) != 9) return(FALSE)

    characters <- c(0:9, LETTERS, "*", "@", "#")

    digits <- unlist(strsplit(toupper(as.character(x)), split = NULL))
    if (any(is.na(match(digits, characters)))) return(FALSE)
    if (!digits[9] %in% 0:9) return(FALSE)

    code <- digits[1:8]
    check <- as.integer(digits[9])

    digits <- unlist(strsplit(code, split = NULL))

    digits <-
      vapply(
        X = digits,
        FUN = function(x) { match(x, characters) - 1L },
        FUN.VALUE = integer(1L)
      )

    digits[c(FALSE, TRUE)] <- digits[c(FALSE, TRUE)] * 2

    digit_sum_mod10 <- function(x) {
        sum(floor(x / 10^(0:(nchar(x) - 1))) %% 10) %% 10
      }
    digits <- vapply(digits, digit_sum_mod10, numeric(1))

    check == (10 - (sum(digits) %% 10)) %% 10
  }

  if (inherits(cusips, "data.frame") && length(cusips) == 1L) {
    cusips <- cusips[[1L]]
  }

  x_factored <- factor(cusips)
  x_levels <- vapply(
    X = levels(x_factored),
    FUN = valid_cusip,
    FUN.VALUE = logical(1),
    USE.NAMES = FALSE
  )
  x_levels[as.numeric(x_factored)]
}
