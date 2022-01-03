is_valid_isin <- function(isins) {
  if (is.data.frame(isins) && identical(length(isins), 1L)) {
    isins <- isins[[1L]]
  }

  is_luhn <- function(x) {
    digits <- suppressWarnings(as.numeric(rev(unlist(strsplit(x, "")))))
    odd <- seq_along(digits) %% 2 == 1
    s1 <- sum(digits[odd])
    s2 <- digits[!odd] * 2
    s2 <- sum(s2 %% 10 + s2 %/% 10)
    sum(s1, s2) %% 10 == 0
  }

  isins <- toupper(isins)
  isins <- gsub(pattern = "[[:blank:]]", replacement = "", isins)
  valid_struct <- grepl("^[[:upper:]]{2}[[:alnum:]]{9}[[:digit:]]$", isins)

  valid_luhn <-
    vapply(
      X = isins,
      FUN = function(x) {
        x <-
          stringi::stri_replace_all_fixed(
            str = x,
            pattern = LETTERS,
            replacement = seq_along(LETTERS) + 9L,
            vectorize_all = FALSE
          )
        out <-
          vapply(
            X = x,
            FUN = is_luhn,
            FUN.VALUE = logical(1L),
            USE.NAMES = FALSE
          )
        out[is.na(out)] <- FALSE
        out
      },
      FUN.VALUE = logical(1),
      USE.NAMES = FALSE
    )

  valid_struct & valid_luhn
}
