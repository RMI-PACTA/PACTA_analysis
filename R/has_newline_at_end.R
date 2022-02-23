has_newline_at_end <- function(filepaths) {
  filepaths <- simplify_if_one_col_df(filepaths)
  stopifnot("`filepaths` must be a character vector" = typeof(filepaths) == "character")
  filepaths <- canonize_path(filepaths)

  vapply(
    X = filepaths,
    FUN = function(filepath) {
      if (!is_file_accessible(filepath) || !is_text_file(filepath)) {
        return(NA)
      }

      con <- file(filepath, "rb")
      on.exit(close(con))

      not_at_end <- TRUE
      chars <- ""
      while (not_at_end) {
        prev_chars <- chars
        chars <- readChar(con, nchars = 2048L, useBytes = TRUE)
        if (length(chars) == 0L) {
          not_at_end <- FALSE
        }
      }

      grepl("[\n\r]$", prev_chars)
    },
    FUN.VALUE = logical(1),
    USE.NAMES = FALSE
  )
}
