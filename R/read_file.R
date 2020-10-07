#' Read files with extension .fst, .csv, and .rda (.rds and .RData)
#'
#' @param file_name Character; path to a file storing a single object.
#'
#' @noRd
#' @examples
#' # Odd case: Read a file saved as .rds but with extension .rda
#' path <- tempfile(fileext = ".rda")
#'
#' data1 <- data.frame(x = 1)
#' saveRDS(data1, path)
#' read_file(path)
#'
#' # More natural case: .RData saved as .rda
#' data2 <- data.frame(x = 1)
#' save(data2, file = path)
#' read_file(path)
#'
#' # Fails by design: .RData with more than 1 object
#'
#' save(data1, data2, file = path)
#' # Fails
#' try(read_file(path))
read_file <- function(path) {
  # FIXME: A warning seems like a bad idea. It should likely be an error
  if (!fs::file_exists(path)) {
    rlang::warn("The `path` does not exist: {path}")
    return(invisible(path))
  }

  abort_unsupported_extension(path)

  extension <- fs::path_ext(path)
  switch(
    extension,
    "rda" = read_rda(path),
    "fst" = fst::read_fst(path),
    "csv" = readr::read_csv(path),
    rlang::abort("Extension is supported but something unexpected happened.")
  )
}

abort_unsupported_extension <- function(path) {
  if (!supported_extension(path)) {
    rlang::abort(glue::glue("Unsupported `path` extension: {path}"))
  }

  invisible(path)
}

supported_extension <- function(path) {
  fs::path_ext(path) %in% c("rda", "csv", "fst")
}

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
    rlang::abort(glue::glue("`path` must have a single object but has {nn}."))
  }

  invisible(dataset)
}
