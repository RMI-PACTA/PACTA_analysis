port_weight_allocation <- function(port_ald) {
  port_ald_pw <- port_ald %>% filter(has_ald_in_fin_sector == TRUE, financial_sector == ald_sector)

  if (data_check(port_ald_pw)) {
    port_ald_pw <- calculate_with_weights(port_ald_pw, "port_weight", "portfolio_weight")
  }

  return(port_ald_pw)
}
