source_web_tool_scripts <- function(x = 1:3) {
  command <- r_script_command(x)

  for (i in seq_along(command)) {
    message("Testing: ", command[[i]])
    stop_on_error(system(command[[i]]))
  }

  invisible(x)
}

r_script_command <- function(x) {
  sprintf("Rscript --vanilla web_tool_script_%s.R TestPortfolio_Input", x)
}

stop_on_error <- function(exit_code) {
  if (exit_code > 0) {
    stop("This script threw an error.", call. = FALSE)
  }

  invisible(exit_code)
}
