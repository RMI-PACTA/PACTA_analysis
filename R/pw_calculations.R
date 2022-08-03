pw_calculations <- function(eq_portfolio, cb_portfolio){

  port_all <- data.frame()

  if(data_check(eq_portfolio)){

    port_all <- bind_rows(port_all, eq_portfolio)
  }

  if(data_check(cb_portfolio)){

    port_all <- bind_rows(port_all, cb_portfolio)

  }

  if(data_check(port_all)){

    port_all <- port_all %>%  select(!!!rlang::syms(grouping_variables),company_id, value_usd)

    port_all <- calculate_port_weight(port_all, grouping_variables)


    pw <- port_all %>%
      group_by(!!!rlang::syms(grouping_variables), company_id) %>%
      summarise(port_weight = sum(port_weight, na.rm = T), .groups = "drop") %>%
      select(company_id, port_weight) %>%
      rename(portfolio_weight = port_weight)

  }else{
    pw <- data.frame(company_id = "No companies in portfolio", portfolio_weight = "0")
  }

  return(pw)
}
