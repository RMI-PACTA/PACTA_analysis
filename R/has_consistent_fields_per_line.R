has_consistent_fields_per_line <- function(filepaths) {
  filepaths <- simplify_if_one_col_df(filepaths)
  stopifnot("`filepaths` must be a character vector" = typeof(filepaths) == "character")

  filepaths <- canonize_path(filepaths)

  vapply(
    X = filepaths,
    FUN = function(filepath) {
      delimiter <- guess_delimiter(filepath)
      fields <-
        readr::count_fields(
          file = filepath,
          tokenizer = readr::tokenizer_delim(delim = delimiter)
        )
      all(fields[[1L]] == fields)
    },
    FUN.VALUE = logical(1L)
  )
}
