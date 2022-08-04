get_portfolio_name <- function() {
  portfolio_name_ref <- commandArgs(trailingOnly = TRUE)

  if (length(portfolio_name_ref) == 0) {
    stop("At least one argument must be supplied (input file).n", call. = FALSE)
  }

  return(portfolio_name_ref)
}
