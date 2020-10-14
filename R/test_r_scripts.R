test_r_scripts <- function(x = 1:3) {
  command <- r_script_command(x)

  for (i in seq_along(command)) {
    message("Testing: ", command[[i]])
    system(command[[i]])
  }

  invisible(x)
}

r_script_command <- function(x) {
  sprintf("Rscript --vanilla web_tool_script_%s.R TestPortfolio_Input", x)
}
