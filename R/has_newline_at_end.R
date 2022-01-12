has_newline_at_end <- function(filepaths) {
  filepaths <- simplify_if_one_col_df(filepaths)
  stopifnot("`filepaths` must be a character vector" = typeof(filepaths) == "character")
  filepaths <- canonize_path(filepaths)

  vapply(
    X = filepaths,
    FUN = function(x) {
      attr(R.utils::countLines(x), "lastLineHasNewline")
    },
    FUN.VALUE = logical(1),
    USE.NAMES = FALSE
  )
}
