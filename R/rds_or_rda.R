#' Determines if a file is a .rda or .rds file
#'
#' This function will determine if a file is a `.rds` or `.rda`/`.RData` file
#' and return `"rds"` or `"rda"` respectively, or return `NA` if it is neither
#' or if the file is inaccessible. It uses the magic bits of the file to make a
#' determination, so it is not dependent on the file name extension, and it
#' automatically adapts to whether the file was saved as binary or ascii and if
#' it is compressed or not.
#'
#' @param filepaths A character vector
#'
#' @return A character vector the same length as `filepaths`.
#'
#' @export
#'
rds_or_rda <- function(filepaths) {
  filepaths <- simplify_if_one_col_df(filepaths)
  stopifnot("`filepaths` must be a character vector" = typeof(filepaths) == "character")
  filepaths <- canonize_path(filepaths)

  vapply(
    X = filepaths,
    FUN = function(filepath) {
      if (is_file_accessible(filepath)) {
        con <- gzfile(filepath, open = "rb")
        on.exit(close(con))

        hdr <- readBin(con, what = "raw", n = 5L)
        hdr_char <- paste(rawToChar(hdr, multiple = TRUE), collapse = "")

        if (grepl("^RD[ABX][2-9]\n", hdr_char)) {
          return("rda")
        } else if (grepl("^[AX][\n\r]", hdr_char)) {
          return("rds")
        }
      }

      NA_character_
    },
    FUN.VALUE = character(1L),
    USE.NAMES = FALSE
  )
}
