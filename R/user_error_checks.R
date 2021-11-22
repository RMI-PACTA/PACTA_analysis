# This function exists to check if a portfolio has a single holdings
# date, or multiple. If there is a single date, then everything is good.
# but on the platform, if a user groups multiple  portfolios, and some of
# these have different holding dates, then the parameters file will
# contain a yaml array with the distinct dates from each grouped
# portfolio.
# Examples:
#
# simple portfolio
# holdings_date: 2020Q4
#
# grouped portfolio with all same date:
# holdings_date: 2020Q4
#
# grouped portfolio with different dates:
# holdings_date: [2019Q4, 2020Q4]
#
# `config` will read this in as a character vector, so all we need to do
# here is check for length > 1
#
#' @examples
#' # No errors
#' check_grouped_portfolio_years("2019Q4")
#' check_grouped_portfolio_years(c("2019Q4"))
#' check_grouped_portfolio_years(c("2019Q4", "2019Q4"))
#' # produces error
#' check_grouped_portfolio_years(c("2019Q4", "2020Q4"))

check_grouped_portfolio_years <- function(
  port_holdings_date
  ) {
  # Extract unique (distinct) values
  hold_date <- unique(port_holdings_date)
  if ((!is.null(hold_date)) && length(hold_date) > 1) {
    error_name <- "Grouped Portfolio with different Holdings Dates"
    desc <- paste(
      "When grouping portfolios, the user included portfolios with",
      "different holdings dates. All portfolios in the group should have",
      "the same holding date (2019Q4, 2020Q4, etc.) The portfolios",
      "in this group have holding dates of:",
      paste0("**", paste(hold_date, collapse = ", "), "**")
    )
    action <- paste(
      "Delete this grouping, and create a new one with portfolios",
      "with the same holdings date before running the analysis again."
    )
    log_user_errors(
      error_name = error_name,
      suggested_action = action,
      description = desc,
      immediate = TRUE
    )
    stop("port_holdings_date contains multiple distinct values", call. = FALSE)
  }
  return(invisible(port_holdings_date))
}
