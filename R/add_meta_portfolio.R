add_meta_portfolio <- function(portfolio, inc_meta_portfolio) {
  portfolio_meta <- portfolio

  # Group at a level

  # lvl <- 1
  #
  #
  # gv <- grouping_variables[1:lvl]
  # ngv <- grouping_variables[seq(lvl+1,length(grouping_variables))]
  #
  # portfolio_1 <- portfolio_meta %>% mutate(!!!rlang::syms(ngv) = "Meta")
  #
  #
  # # loop through grouping variables
  # # the order of gv defines this
  #
  # for (g in length(grouping_variables)){
  #
  #   gv <- grouping_variables[g]
  #
  #   # portfolio_meta_sub <- [,gv]
  #
  #   portfolio_0 <- portfolio %>%
  #     mutate(country_group = "meta",
  #            investor_name = "meta",
  #            portfolio_name = "meta")
  #
  #   portfolio_1 <- portfolio %>%
  #     group_by(country_group) %>%
  #     mutate(investor_name = paste(country_group, "meta"),
  #     portfolio_name = paste(country_group, "meta"))
  #
  #   portfolio_2 <- portfolio %>%
  #     group_by(country_group, investor_name) %>%
  #     mutate(portfolio_name = paste(investor_name, "meta"))
  #
  #   portfolio_all <- bind_rows(portfolio,portfolio_0, portfolio_1, portfolio_2)
  #
  # }

  if (inc_meta_portfolio) {
    portfolio_meta$portfolio_name <- meta_portfolio_name
    portfolio_meta$investor_name <- meta_investor_name
    portfolio <- rbind(portfolio, portfolio_meta)
  }

  portfolio
}
