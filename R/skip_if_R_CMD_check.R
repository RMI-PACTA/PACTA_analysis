skip_if_R_CMD_check <- function() {
  if (!nzchar(Sys.getenv("R_CMD"))) {
    return(invisible(TRUE))
  }

  testthat::skip("Not run in R CMD check")
}
