quit_if_no_pacta_relevant_data <- function(portfolio) {
  if (!any(portfolio$has_asset_level_data, na.rm = TRUE)) {
    error_name <- "Portfolio does not have any holdings with PACTA relevant data"

    desc <- paste(
      "This portfolio does not have any direct or indirect investments in ",
      "companies for which we have PACTA relevant, asset based company data."
    )

    action <- paste(
      "Try uploading a portfolio with PACTA relevant holdings."
    )

    log_user_errors(
      error_name = error_name,
      suggested_action = action,
      description = desc,
      immediate = TRUE
    )

    if (!interactive()) {
      # using message and quit, rather than stop(), because server silently fails
      # if docker container exits with anything other than a 0 status code
      message("Portfolio does not have any holdings with PACTA relevant data")
      quit(save = "no", status = 0L, runLast = FALSE)
    }

    stop("no PACTA relevant data")
  }
}
