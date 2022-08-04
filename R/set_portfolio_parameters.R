set_portfolio_parameters <- function(file_path) {
  cfg <- config::get(file = file_path)

  null_or_string <- function(x) if (is.null(x)) { NULL } else { as.character(x) }

  portfolio_name <<- null_or_string(cfg$parameters$portfolio_name)
  investor_name <<- null_or_string(cfg$parameters$investor_name)
  peer_group <<- null_or_string(cfg$parameters$peer_group)
  language_select <<- null_or_string(cfg$parameters$language)
  user_id <<- null_or_string(cfg$parameters$user_id)
  project_code <<- null_or_string(cfg$parameters$project_code)
  port_holdings_date <<- null_or_string(cfg$parameters$holdings_date)
}

add_naming_to_portfolio <- function(portfolio_raw) {
  portfolio_raw$portfolio_name <- portfolio_name
  portfolio_raw$investor_name <- investor_name

  return(portfolio_raw)
}
