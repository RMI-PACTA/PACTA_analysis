canonize_path <- function(path = ".") {
  vapply(
    X = path,
    FUN = function(single_path) {
      if (!is.character(single_path)) {
        return(NA_character_)
      }
      fs::path_abs(fs::path_expand(enc2utf8(single_path)))
    },
    FUN.VALUE = character(1L),
    USE.NAMES = FALSE
  )
}
