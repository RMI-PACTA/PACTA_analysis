#' Run PACTA for all portfolios matching a pattern in an input directory
#'
#' @param path String. Directory where portfolios are located.
#' @param regext String. Regular expression that defines a valid portfolio name.
#'
#' @examples
#' run_pacta()
#' @noRd
run_pacta <- function(path = "../input", regexp = "_Input[.]csv") {
  portfolios <- portfolios(path, regexp)
  pacta_source <- write_pacta_source()
  command <- glue::glue("Rscript --vanilla {pacta_source} {portfolios}")

  for (i in seq_along(command)) {
    message("Start portfolio: ", portfolios[[i]])
    system(command[[i]])
    message("End portfolio: ", portfolios[[i]])
  }
}

#' Get the name of the portfolios
#' @inheritParams run_pacta
#' @return A character vector.
#' @examples
#' portfolios()
#'
#' input <- tempdir()
#' fs::file_create(fs::path(input, "my_portfolio.csv"))
#' portfolios(input, regexp = "_portfolio[.]csv")
#' @noRd
portfolios <- function(path = "../input", regexp = "_Input[.]csv") {
  csv <- fs::dir_ls(path, regexp = regexp)
  fs::path_ext_remove(fs::path_file(csv))
}

#' Write the source code of PACTA to a file
#'
#' Pick the source code of PACTA only -- excluding everything downstream.
#'
#' @param destfile String. Path to a destination file.
#' @param end String. Mark of the end of PACTA code.
#'
#' @return Called for its side effect. Returns the first argument invisibly.
#'
#' @examples
#' destfile <- write_pacta_source()
#' writeLines(readLines(destfile))
#' @noRd
write_pacta_source <- function(destfile = tempfile(), end = "# pacta: end") {
  scripts <- sprintf("web_tool_script_%s.R", 1:2)
  lines <- Reduce(c, lapply(scripts, readLines))
  end <- which(grepl(end, lines))
  pacta_source <- lines[1:end]

  writeLines(pacta_source, destfile)
  invisible(destfile)
}
