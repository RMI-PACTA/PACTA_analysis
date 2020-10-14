#' Style .R and .Rmd files
#'
#' To style only R-package sources useuse_tidy_style()
#'
#' @examples
#' style_all()
#' @noRd
style_all <- function(pattern = c("[.]R$", "[.]Rmd$")) {
  lapply(pattern, style_pattern)

  invisible(pattern)
}

style_pattern <- function(pattern) {
  files <- list.files(pattern = pattern, recursive = TRUE)
  lapply(files, styler::style_file)

  invisible(pattern)
}
