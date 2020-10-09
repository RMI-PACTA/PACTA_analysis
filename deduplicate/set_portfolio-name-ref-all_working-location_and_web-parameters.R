if (in_transitionmonitor()) {
  portfolio_name_ref_all <- get_portfolio_name()
  working_location <- getwd()
  set_web_parameters(file_path = paste0(working_location, "/parameter_files/WebParameters_docker.yml"))
} else {
  portfolio_name_ref_all <- c("TestPortfolio_Input") # must be the same name as in the _PortfolioParameters.yml
  working_location <- here::here()
  set_web_parameters(file_path = paste0(working_location, "/parameter_files/WebParameters_2dii.yml"))
}
