convert_industry_classification <-
  function(data, from, to) {
    sector_bridge <- get_sector_bridge()
    stopifnot(to %in% names(sector_bridge))

    dict <- sector_bridge[sector_bridge$source == from, ]
    dict <- dict[c("industry_classification", to)]
    fast_match(x = data, dict = dict)
  }


sector_from_bics_subgroup <-
  function(data) {
    convert_industry_classification(data, from = "BICS", to = "sector")
  }


sector_from_icb_subgroup <-
  function(data) {
    convert_industry_classification(data, from = "ICB", to = "sector")
  }
