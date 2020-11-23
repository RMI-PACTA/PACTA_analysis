convert_industry_classification <-
  function(.data, from, to) {
    sector_bridge <- get_sector_bridge()
    stopifnot(to %in% names(sector_bridge))

    dict <- sector_bridge[sector_bridge$source == from, ]
    dict <- dict[c("industry_classification", to)]
    fast_match(x = .data, dict = dict)
  }


convert_bclass_to_sector <-
  function(.data) {
    convert_industry_classification(.data, from = "BCLASS", to = "sector")
  }


convert_bics_to_sector <-
  function(.data) {
    convert_industry_classification(.data, from = "BICS", to = "sector")
  }


convert_icb_to_sector <-
  function(.data) {
    convert_industry_classification(.data, from = "ICB", to = "sector")
  }


convert_bics_subgroup_to_bics_sector <-
  function(.data) {
    bics_bridge <- get_bics_bridge_data()

    fast_match(x = .data, dict = bics_bridge[2:1])
  }
