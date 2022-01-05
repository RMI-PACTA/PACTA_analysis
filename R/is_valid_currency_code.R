#' Validate a vector of currency codes
#'
#' This function validates that a vector of currency codes are valid currency
#' codes that exist in the ISO 4217 alpha code specification.
#'
#' @param currency_codes A character vector
#'
#' @return A logical vector the same length as `currency_codes`.
#'
#' @importFrom stats na.omit
#' @export
#'
is_valid_currency_code <- function(currency_codes) {
  if (is.data.frame(currency_codes) && identical(length(currency_codes), 1L)) {
    currency_codes <- currency_codes[[1L]]
  }

  vapply(
    X = currency_codes,
    FUN = function(currency_code) {
      toupper(currency_code) %in% na.omit(countrycode::codelist$iso4217c)
    },
    FUN.VALUE = logical(1L),
    USE.NAMES = FALSE
  )
}
