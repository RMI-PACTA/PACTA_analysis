is_valid_currency_code <- function(currency_codes) {
  if (is.data.frame(currency_codes) && identical(length(currency_codes), 1L)) {
    currency_codes <- currency_codes[[1L]]
  }

  vapply(
    X = currency_codes,
    FUN = function(currency_code) {
      toupper(currency_code) %in% countrycode::codelist$iso4217c
    },
    FUN.VALUE = logical(1),
    USE.NAMES = FALSE
  )
}
