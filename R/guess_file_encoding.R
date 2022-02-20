#' Guess the file encoding for a vector of filenames or filepaths
#'
#' This function will guess the file encoding of a vector of filenames or
#' filepaths and return the file encoding as a string. It primarily uses
#' `stringi::stri_enc_detect()` to guess the encoding. Additionally, it
#' searches for known CP850 and CP1252 characters and will return the
#' appropriate encoding if found, because ICU/stringi cannot detect them. If a
#' file is a binary file, it will return `"binary"`. If a file is inaccessible
#' it will return `NA` for that element.
#'
#' @param filepaths A character vector
#' @param threshold A single element numeric (minimum confidence level of the guess \[0-1\])
#'
#' @return A character vector the same length as `filepaths`.
#'
#' @export
#'
guess_file_encoding <- function(filepaths, threshold = 0.2) {
  filepaths <- simplify_if_one_col_df(filepaths)
  stopifnot("`filepaths` must be a character vector" = typeof(filepaths) == "character")
  filepaths <- canonize_path(filepaths)

  vapply(
    X = filepaths,
    FUN = function(filepath) {
      if (!is_file_accessible(filepath)) {
        return(NA_character_)
      }
      if (!is_text_file(filepath)) {
        return("binary")
      }

      lines <- stringi::stri_read_raw(filepath)

      if (all(stringi::stri_enc_isascii(lines))) {
        return("ascii")
      }

      # Swiss - CP850
      # u-umlaut - 0x81 - "\x81" - iconv("\x81", "cp850", "UTF-8") - validUTF8("\x81")
      # o-umlaut - 0x94 - "\x94" - iconv("\x94", "cp850", "UTF-8") - validUTF8("\x94")
      if (any(lines == as.raw(0x81)) || any(lines == as.raw(0x94))) {
        return("cp850")
      }

      # Norwegian - CP1252
      # slashed o - 0xf8 - "\xf8" - iconv("\xf8", "cp1252", "UTF-8") - validUTF8("\xf8")
      if (any(lines == as.raw(0xf8))) {
        return("cp1252")
      }

      guess <- stringi::stri_enc_detect(lines)[[1]]
      guess <- guess[guess$Confidence > threshold, "Encoding"]

      if (length(guess) == 0) {
        return("unknown")
      }

      guess[[1]]
    },
    FUN.VALUE = character(1L),
    USE.NAMES = FALSE
  )
}
