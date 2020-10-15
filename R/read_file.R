#' Read .rda files files with supported extensions
#'
#' @param path Character; path to a file storing a single object.
#'
#' @examples
#' # Odd case: Read a file saved as .rds but with extension .rda
#' path <- tempfile(fileext = ".rda")
#'
#' data1 <- data.frame(x = 1)
#' saveRDS(data1, path)
#' read_rda(path)
#'
#' # More natural case: .RData saved as .rda
#' data2 <- data.frame(x = 1)
#' save(data2, file = path)
#' read_rda(path)
#'
#' # Fails by design: .RData with more than 1 object
#'
#' save(data1, data2, file = path)
#' # Fails
#' try(read_rda(path))
#' @noRd
read_rda <- function(file_name) {
  tryCatch(
    # Unfortunately, legacy code saved .rds files with the extension .rda
    readr::read_rds(file_name),
    error = function(e) read_rdata(file_name)
  )
}

read_rdata <- function(path) {
  e <- new.env()
  load(path, envir = e)
  dataset <- ls(envir = e)

  abort_if_too_long(dataset)

  e[[dataset]]
}

abort_if_too_long <- function(dataset) {
  nn <- length(dataset)
  if (nn > 1L) {
    abort(glue("`path` must have a single object but has {nn}."))
  }

  invisible(dataset)
}
