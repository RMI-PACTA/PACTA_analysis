canonize_path <- function(path = ".") {
  vapply(
    X = path,
    FUN = function(x) {
      if (!is.character(x)) {
        return(NA_character_)
      }
      fs::path_abs(fs::path_expand(enc2utf8(x)))
    },
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )
}
