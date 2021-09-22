#' Run PACTA for all portfolios matching a pattern in an input directory
#'
#' @param wd String. Working directory pointing to pacta's source code
#' @param regext String. Regular expression that defines a valid portfolio name.
#'
#' @examples
#' run_pacta()
#' @noRd
run_pacta <- function(wd = here::here()) {
  withr::local_dir(wd)

  portfolios <- portfolios()
  command <- glue::glue("Rscript --vanilla web_tool_script_1.R {portfolios};\
                          Rscript --vanilla web_tool_script_2.R {portfolios}")

  for (i in seq_along(command)) {
    message("Start portfolio: ", portfolios[[i]])
    system(command[[i]])
    message("End portfolio: ", portfolios[[i]])
  }
}

#' Get the name of the portfolios
portfolios <- function(path = "../input", regexp = "_Input[.]csv") {
  csv <- fs::dir_ls(path, regexp = regexp)
  fs::path_ext_remove(fs::path_file(csv))
}

