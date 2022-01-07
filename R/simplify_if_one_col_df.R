simplify_if_one_col_df <- function(obj) {
  if (is.data.frame(obj) && identical(length(obj), 1L)) {
    return(obj[[1L]])
  }
  obj
}
