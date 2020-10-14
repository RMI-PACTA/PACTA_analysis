test_r_scripts <- function(x = 1:3) {
  command <- glue("Rscript --vanilla web_tool_script_{x}.R TestPortfolio_Input")
  purrr::walk(command, system)

  invisible(x)
}
