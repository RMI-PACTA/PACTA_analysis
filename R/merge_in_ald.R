merge_in_ald <- function(portfolio, ald_scen) {
  portfolio <- portfolio %>% left_join(ald_scen, by = "id")

  return(portfolio)
}
