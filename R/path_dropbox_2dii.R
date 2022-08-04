# reimplement r2dii.utils' path_dropbox_2dii() so r2dii.utils is not a dependency
path_dropbox_2dii <- function(...) {
  custom <- getOption("r2dii_dropbox")
  default <- "Dropbox (2\u00B0 Investing)"
  path <- ifelse(is.null(custom), default, custom)
  path.expand(file.path("~", path, ...))
}
