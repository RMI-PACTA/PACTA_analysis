convert_quarter_to_year <- function(quarter_string){
  # extract unique values, so that even if an array is passed in with all
  # same values, we still accept it, if all values are the same.
  quarter_string <- unique(quarter_string)
  # see definition of check_grouped_portfolio_years for more info, but
  # this give the user a useful error if length > 1 (grouped portfolio
  # from multiple timestamps)
  check_grouped_portfolio_years(quarter_string)
  # check that it's a valid timestamp xxxxQy
  stopifnot(
    grepl(
      pattern = "^[[:digit:]]{4}Q[1-4]$",
      x = quarter_string,
      ignore.case = TRUE
    )
  )
  year_string <- gsub(
    pattern = "Q[[:digit:]]",
    replacement = "",
    x = port_holdings_date,
    ignore.case = TRUE
  )
  # coerce to integer
  year_int <- as.integer(year_string)
  return(year_int)
}
