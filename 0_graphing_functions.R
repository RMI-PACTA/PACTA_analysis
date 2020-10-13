wrap_it <- function(x, len) {
  sapply(x, function(y) paste(strwrap(y, len), collapse = "\n"), USE.NAMES = FALSE)
}

wrap_labels <- function(x, len) {
  if (is.list(x)) {
    lapply(x, wrap_it, len)
  } else {
    wrap_it(x, len)
  }
}

comprss <- function(tx) {
  tx[is.na(tx)] <- 0
  div <- findInterval(tx, c(1, 1e3, 1e6, 1e9, 1e12))
  div[div == 0] <- 1
  labels <- paste(
    "$", round(tx / 10^(3 * (div - 1)), 2),
    c("", "K", "Mn", "Bn", "Tn")[div]
  )

  return(labels)
}

comprss_long <- function(tx) {
  tx[is.na(tx)] <- 0
  div <- findInterval(tx, c(1, 1e3, 1e6, 1e9, 1e12))
  div[div == 0] <- 1
  labels <- paste(
    round(tx / 10^(3 * (div - 1)), 1),
    c("", thousand, million, billion, trillion)[div]
  )
  return(labels)
}

perc <- function(x, format = "f", ...) {
  x <- ifelse(x * 100 >= 10,
    paste0(" ", formatC(100 * x, format = format, digits = 1, ...), "%"),
    paste0(formatC(100 * x, format = format, digits = 2, ...), "%")
  )


  return(x)
}

shortened_company_names <- function(company_labels, length) {
  for (i in 1:length(company_labels)) {
    if (str_length(company_labels[[i]]) > length) {
      new_name <- strtrim(company_labels[[i]], length)
      company_labels[[i]] <- paste0(new_name, "...")
    } else if (str_length(company_labels[[i]]) < length) {
      for (j in 1:((length + 3) - str_length(company_labels[[i]]))) {
        company_labels[[i]] <- paste0(" ", company_labels[[i]])
      }
    }
  }
  company_labels
}

set_initial_variables <- function() {
  EQCombin <<- NA
  CBCombin <<- NA

  EQCompProdSnapshot <<- NA
  CBCompProdSnapshot <<- NA

  EQportmap <<- NA
  CBportmap <<- NA

  eq_market <<- NA
  cb_market <<- NA

  has_equity <<- FALSE
  has_debt <<- FALSE

  HasCarbonBudget <<- FALSE

  RiskToPlot <<- NA
  PlotChart <<- TRUE

  chart_type <<- ""
  tech_to_plot <<- ""
  sector_to_plot <<- ""
  BV.asset_type <<- ""

  HasRenCapCB <<- F
  HasRenCapEQ <<- F

  HasCoalCapCB <<- F
  HasCoalCapEQ <<- F

  HasPower <<- FALSE
  HasAuto <<- FALSE
  HasOG <<- FALSE
  HasCoal <<- FALSE

  HasPowerCB <<- FALSE
  HasAutoCB <<- FALSE
  HasOGCB <<- FALSE
  HasCoalCB <<- FALSE

  HasPowerEQ <<- FALSE
  HasAutoEQ <<- FALSE
  HasOGEQ <<- FALSE
  HasCoalEQ <<- FALSE

  SovBondCov <<- NA
}

define_peers <- function() {
  if (twodii_internal) {
    if (file.exists(paste0(results_path, "/Equity_results_portfolio.rda"))) {
      eq_peers <<- read_rds(paste0(results_path, "/Equity_results_portfolio.rda")) %>%
        filter(investor_name == meta_investor_name)
    } else {
      warning("No Equity Peers File available. Setting inc_meta_portfolio to true MAY solve this. Alternatively there is no equity in any portfolio.")
    }
    if (file.exists(paste0(results_path, "/Bonds_results_portfolio.rda"))) {
      cb_peers <<- read_rds(paste0(results_path, "/Bonds_results_portfolio.rda")) %>%
        filter(investor_name == meta_investor_name)
    } else {
      warning("No bonds peers file available. Setting inc_meta_portfolio to true MAY solve this. Alternatively there are no bonds in any portfolio.")
    }
  } else {
    if (project_name == "web_tool") {
      eq_peers <<- NA

      cb_peers <<- NA
    } else {
      eq_peers <<- read_rds(paste0(project_location_ext, "/", project_name, "/40_Results/Equity_results_portfolio.rda")) %>%
        filter(investor_name == meta_investor_name)

      cb_peers <<- read_rds(paste0(project_location_ext, "/", project_name, "/40_Results/Bonds_results_portfolio.rda")) %>%
        filter(investor_name == meta_investor_name)
    }
  }
}

define_benchmarks <- function() {
  if (twodii_internal) {
    eq_market <<- read_rds(paste0(portcheck_v2_path, "/10_Projects/INDEX_2019/40_Results/Equity_results_portfolio.rda")) %>%
      filter(portfolio_name == eq_market_ref)
    cb_market <<- read_rds(paste0(portcheck_v2_path, "/10_Projects/INDEX_2019/40_Results/Bonds_results_portfolio.rda")) %>%
      filter(portfolio_name == cb_market_ref)
  } else {

    # Set per project
    eq_market <<- read_rds(paste0(data_location_ext, "/Fake_Index/Equity_results_portfolio.rda"))

    cb_market <<- read_rds(paste0(data_location_ext, "/Fake_Index/Bonds_results_portfolio.rda"))
  }

  if (!start_year %in% eq_market$year) {
    stop(paste0("Check that start years align. EQ Market Start year = ", min(eq_market$year), ". Report start year = ", start_year))
  }
  if (!start_year %in% cb_market$year) {
    stop(paste0("Check that start years align. CB Market Start year = ", min(cb_market$year), ". Report start year = ", start_year))
  }
}

results_call <- function() {

  # Filtering Market should already happen here.


  subgroup_overview <<- portfolio_overview[portfolio_overview$investor_name == investor_name_select & portfolio_overview$portfolio_name == portfolio_name_select & portfolio_overview$valid_input == TRUE, ]


  define_benchmarks()

  define_peers()

  # RenewableAdditions <<- readRDS(paste0(analysis_inputs_path, "/RenewablesAdditionsData.rda"))
  # CoalRetirements <<- readRDS(paste0(analysis_inputs_path, "/CoalRetirementsData.rda"))
  # CarbonData <<-  readRDS(paste0(analysis_inputs_path, "/CarbonCapexUpstream.rda"))

  if (has_equity) {
    EQCompProdSnapshot <<- read_rds(paste0(results_path, "/", investor_name_select, "/Equity_results_company.rda"))
    EQCompProdSnapshot <<- EQCompProdSnapshot[EQCompProdSnapshot$portfolio_name == portfolio_name_select, ]

    if (has_map) {
      if (file.exists(paste0(results_path, "/", investor_name_select, "/Equity_results_map.rda"))) {
        EQportmap <- readRDS(paste0(results_path, "/", investor_name_select, "/Equity_results_map.rda"))
        EQportmap <- EQportmap %>% filter(portfolio_name == portfolio_name_select)
      }
    }


    if (data_check(EQportmap)) {
      EQportmap$scenario_geography <- ifelse(EQportmap$ald_sector == "Power", "GlobalAggregate", "Global")
    }

    EQportmap <<- EQportmap

    EQTechData <<- read_rds(paste0(analysis_inputs_path, "/masterdata_ownership_datastore_technology_type_view.rda"))

    EQOilShareData <<- EQTechData %>%
      select(id, ald_sector, technology, technology_type, year, ald_production, ald_production_unit) %>%
      filter(
        year %in% seq(start_year, start_year + 5, 1),
        ald_sector == "Oil&Gas"
      ) %>%
      mutate(technology = if_else(technology == "Oil and Condensate", "Oil", technology))

    # EQOilShareData <<- readRDS(paste0(analysis_inputs_path, "oil_and_gas_resource_type_rollup_ownership.rda"))
    # EQOilShareData <<- readRDS(paste0(Location, "ReferenceData/oil_and_gas_resource_type_rollup_ownership.rda"))
  }

  if (has_debt == TRUE) {
    CBCompProdSnapshot <<- readRDS(paste0(results_path, "/", investor_name_select, "/Bonds_results_company.rda"))
    CBCompProdSnapshot <<- CBCompProdSnapshot[CBCompProdSnapshot$portfolio_name == portfolio_name_select, ]

    if (has_map) {
      if (file.exists(paste0(results_path, "/", investor_name_select, "/Bonds_results_map.rda"))) {
        CBportmap <<- readRDS(paste0(results_path, "/", investor_name_select, "/Bonds_results_map.rda"))
        CBportmap <<- CBportmap %>% filter(portfolio_name == portfolio_name_select)
      }
      if (data_check(CBportmap)) {
        CBportmap$scenario_geography <- ifelse(CBportmap$ald_sector == "Power", "GlobalAggregate", "Global")
      }
      CBportmap <<- CBportmap
    }

    CBTechData <<- read_rds(paste0(analysis_inputs_path, "/masterdata_debt_datastore_technology_type_view.rda"))

    CBOilShareData <<- CBTechData %>%
      select(id, ald_sector, technology, technology_type, year, ald_production, ald_production_unit) %>%
      filter(
        year %in% seq(start_year, start_year + 5, 1),
        ald_sector == "Oil&Gas"
      ) %>%
      mutate(technology = if_else(technology == "Oil and Condensate", "Oil", technology))
  }

  if (has_sb) {
    SB.Summary <<- read.csv(paste0(PROC.INPUT.PATH, "SovereignBondSummary.csv"), strip.white = T, stringsAsFactors = F)
  }

  if (inc_sda_approach) {
    if (data_check(EQCombin)) {
      max_market <- max(eq_market$year, na.rm = T)
      max_year_data <- max(EQCombin$year, na.rm = T)

      if (max_year_data == max_market) {
        EQCombin <- sda_portfolio_target(eq_market, EQCombin,
          start_year = start_year,
          target_year = max_year_data
        )
      }
    }



    if (data_check(CBCombin)) {
      max_market <- max(cb_market$year, na.rm = T)
      max_year_data <- max(CBCombin$year, na.rm = T)

      if (max_year_data == max_market) {
        CBCombin <- sda_portfolio_target(cb_market, CBCombin,
          start_year = start_year,
          target_year = max_year_data
        )
      }
    }
  }
}

filter_by_parameters <- function(df, dfType, byscenario = TRUE, scenario.irrelevant = FALSE, by_equity_market = TRUE, ActorSectorOnly = FALSE, BV.asset_type = "") {

  # This Df type should allow us to clear a few things up

  if (data_check(df) == FALSE) {
    print("No Results")
    PlotChart <<- FALSE
  } else {
    PlotChart <<- TRUE

    if ("asset_type" %in% colnames(df) & BV.asset_type != "") {
      df <- df %>% filter(asset_type %in% BV.asset_type)
      if (data_check(df) == FALSE) {
        print("BV.asset_type Filter removed All Data")
      }
    }

    if (tech_to_plot != "" & "technology" %in% colnames(df)) {
      if (GraphType %in% c("CurrentTechnologyExposure", "FutureTechnologyExposure") & sector_to_plot %in% c("Oil&Gas", "Coal")) {
        df <- df %>% filter(technology %in% c("Oil", "Gas", "Coal"))
        if (data_check(df) == FALSE) {
          print("Techology Filter removed All Data")
        }
      } else {
        df <- df %>% filter(technology == tech_to_plot)
      }
      if (data_check(df) == FALSE) {
        print("Techology Filter removed All Data")
      }
    }

    if (RiskToPlot != "" & "RiskType" %in% colnames(df)) {
      df <- df %>% filter(RiskType == RiskToPlot)
      if (data_check(df) == FALSE) {
        print("Risk Filter removed All Data")
      }
    }

    # if (EquityMarketSelection != "" & "equity_market" %in% colnames(df) & by_equity_market == TRUE & data_check(df) == TRUE){df <- df %>% filter(equity_market == EquityMarketSelection)
    if (EquityMarketSelection != "" & "equity_market" %in% colnames(df) & by_equity_market == TRUE & data_check(df) == TRUE) {
      df <- df %>% filter(equity_market == EquityMarketSelection)
      if (data_check(df) == FALSE) {
        print("Equity Market Filter removed All Data")
      }
    }


    if (!AccountingPrinciple %in% c("", "Mixed") & "allocation" %in% colnames(df) & data_check(df) == TRUE) { # AccountingPrinciple %in% df$allocation &
      df <- df %>% filter(allocation == AccountingPrinciple)
      if (data_check(df) == FALSE) {
        print("Accounting Principle Filter removed All Data")
      }
    }

    if (AccountingPrinciple == "Mixed" & "allocation" %in% colnames(df) & data_check(df) == TRUE) {
      if (dfType == "EQ") {
        df <- df %>% filter(allocation == "ownership_weight")
      } else if (dfType == "CB") {
        df <- df %>% filter(allocation == "portfolio_weight")
      }
      if (data_check(df) == FALSE) {
        print("Accounting Principle Filter removed All Data")
      }
    }


    if (ScenarioGeographyChoose != "" & "scenario_geography" %in% colnames(df) & data_check(df) == TRUE) {
      if (ScenarioGeographyChoose == "GlobalAggregate") {
        df1 <- df %>% filter(ald_sector == "Power", scenario_geography == "GlobalAggregate")
        if (!data_check(df1)) {
          df1 <- df %>% filter(ald_sector == "Power", scenario_geography == "Global")
        }
        df2 <- df %>% filter(ald_sector != "Power", scenario_geography == "Global")
        df <- rbind(df1, df2)
      } else {
        df <- df %>% filter(scenario_geography == ScenarioGeographyChoose)
      }
      if (data_check(df) == FALSE) {
        print("scenario_geography Filter removed All Data")
      }
    }


    if (!sector_to_plot %in% c("", "All") & data_check(df) == TRUE) {
      if (GraphType %in% c("CurrentTechnologyExposure", "FutureTechnologyExposure") & sector_to_plot %in% c("Oil&Gas", "Coal")) {
        df <- df %>% filter(ald_sector %in% c("Oil&Gas", "Coal"))
      } else {
        df <- df %>% filter(ald_sector == sector_to_plot)
      }

      if (ActorSectorOnly == TRUE & !sector_to_plot %in% c("", "All") & data_check(df) == TRUE) {
        df <- df %>% filter(financial_sector == sector_to_plot)
      }
      if (data_check(df) == FALSE) {
        print("ald_sector Filter removed All Data")
      }
    }


    # if (AccountingPrinciple != "" & "allocation" %in% colnames(df) & AccountingPrinciple %in% df$allocation & data_check(df) == TRUE){df <- df %>% filter(allocation == AccountingPrinciple)
    # if (data_check(df) == FALSE){print("Accounting Principle Filter removed All Data")}}


    if (byscenario == TRUE & Scenariochoose != "" & "scenario" %in% colnames(df) & Scenariochoose %in% df$scenario & data_check(df) == TRUE) {
      df <- df %>% filter(scenario == Scenariochoose)
      if (data_check(df) == FALSE) {
        print("scenario Filter removed All Data")
      }
    }

    if (byscenario == TRUE & scenario.irrelevant == TRUE & "scenario" %in% colnames(df) & data_check(df) == TRUE) {
      # df$scenario <- NULL
      df <- df[df$scenario == unique(df$scenario)[1], ]
      df$scenario <- "NA"
      if (data_check(df) == FALSE) {
        print("scenario Filter removed All Data")
      }
    }

    if (PeerGroupSelection != "" & "investor_name" %in% colnames(df) & PeerGroupSelection %in% df$investor_name & data_check(df) == TRUE) {
      df <- df %>% filter(investor_name == PeerGroupSelection)
      if (data_check(df) == FALSE) {
        print("Peer Group Filter removed All Data")
      }
    }


    if (GraphType %in% c("FutureTechnologyExposure", "Map")) {
      if ("year" %in% colnames(df)) {
        df <- df %>% filter(year == start_year + 5)
      }
    } else if (GraphType %in% c("CurrentTechnologyExposure")) {
      if ("year" %in% colnames(df)) {
        df <- df %>% filter(year == start_year)
      }
    }

    if (GraphType %in% "CompanyInformation") {
      if (sector_to_plot %in% c("Oil&Gas", "CoalMining")) {
        if ("year" %in% colnames(df)) {
          df <- df %>% filter(year %in% c(start_year, start_year + 5))
        }
      } else {
        if ("year" %in% colnames(df)) {
          df <- df %>% filter(year == start_year + 5)
        }
      }
    }

    if (GraphType %in% c("PeerComparison")) {
      if (PeerGroupSelection != "" & "portfolio_name" %in% colnames(df) & "PeerGroup" %in% colnames(df) & data_check(df) == TRUE) {
        df <- df %>% filter(portfolio_name != "MetaPort")
        if (data_check(df) == FALSE) {
          print("Peer Group Sub-Filter removed All Data")
        }
      }
    } else {
      if (PeerGroupSelection != "" & "portfolio_name" %in% colnames(df) & "PeerGroup" %in% colnames(df) & data_check(df) == TRUE) {
        df <- df %>% filter(portfolio_name == "MetaPort")
        if (data_check(df) == FALSE) {
          print("Peer Group Sub-Filter removed All Data")
        }
      }
    }

    if (data_check(df) == FALSE) {
      PlotChart <<- FALSE
    }
  }

  return(df)
} # finalize and adjust for future & current exposure

create_test_list <- function() {
  # ID.COLS <- c("PortName","investor_name_select","Type") # removed Type as this is not existing
  ID.COLS <- c("investor_name", "portfolio_name")
  ID.COLS.LONG <- c("investor_name", "portfolio_name", "year", "allocation", "ald_sector", "technology", "scenario_geography")
  # test_list <- NA

  ald_sectors <- c("Automotive", "Coal", "OilGas", "Power")

  if (file.exists(paste0(results_path, "/", investor_name_select, "/Equity_results_portfolio.rda"))) {
    EQCombin <<- read_rds(paste0(results_path, "/", investor_name_select, "/Equity_results_portfolio.rda"))
    EQCombin <<- EQCombin %>% filter(portfolio_name == portfolio_name_select)
    # EQCombin
  }
  if (file.exists(paste0(results_path, "/", investor_name_select, "/Bonds_results_portfolio.rda"))) {
    CBCombin <<- read_rds(paste0(results_path, "/", investor_name_select, "/Bonds_results_portfolio.rda"))
    CBCombin <<- CBCombin %>% filter(portfolio_name == portfolio_name_select)
  }

  if (data_check(EQCombin) == T) {
    # if (sector_to_plot %in% c("Shipping"))

    Equity <- filter_by_parameters(EQCombin, "EQ", ActorSectorOnly = T) %>%
      select(investor_name, portfolio_name, year, scenario, allocation, ald_sector, technology, scenario_geography, equity_market, plan_tech_prod) %>%
      group_by(investor_name, portfolio_name, scenario_geography, allocation, equity_market, year, ald_sector, technology) %>%
      summarise(Production = sum(plan_tech_prod, na.rm = T))

    EquityTech <- Equity %>%
      ungroup() %>%
      select(investor_name, portfolio_name, technology, Production) %>%
      group_by(investor_name, portfolio_name, technology) %>%
      summarise(ald_sectorProd = sum(Production, na.rm = T))

    Equity <- Equity %>%
      ungroup() %>%
      select(investor_name, portfolio_name, ald_sector, Production) %>%
      group_by(investor_name, portfolio_name, ald_sector) %>%
      summarise(ald_sectorProd = sum(Production, na.rm = T))

    if (nrow(Equity) > 0) {
      Equity$ald_sector <- ifelse(Equity$ald_sector == "Oil&Gas", "OilGas", Equity$ald_sector)
      Equity$Indicator <- ifelse(Equity$ald_sectorProd > 0, TRUE, FALSE)
      Equity$ald_sectorProd <- NULL
      Equity$ald_sector <- paste0(Equity$ald_sector, ".EQ")
      Equity <- Equity %>% spread(value = Indicator, key = ald_sector)
      Equity <<- as.data.frame(Equity)

      HasCoalCapEQ <<- "CoalCap" %in% EquityTech$technology
      HasRenCapEQ <<- "RenewableCap" %in% EquityTech$technology

      has_equity <<- TRUE
    } else {
      has_equity <<- FALSE
    }
  }


  if (all(data_check(CBCombin) == T, chart_type %in% c("CB", "") | GraphType == "PercentageOfPortfolioAssessed")) {
    Debt <- filter_by_parameters(CBCombin, "CB", by_equity_market = F, ActorSectorOnly = T) %>%
      select(investor_name, portfolio_name, year, scenario, allocation, ald_sector, technology, scenario_geography, equity_market, plan_tech_prod) %>%
      # filter(year == start_year) %>%
      group_by(investor_name, portfolio_name, scenario_geography, allocation, equity_market, year, ald_sector, technology) %>%
      summarise(Production = sum(plan_tech_prod, na.rm = T))

    DebtTech <- Debt %>%
      ungroup() %>%
      select(investor_name, portfolio_name, technology, Production) %>%
      group_by(investor_name, portfolio_name, technology) %>%
      summarise(ald_sectorProd = sum(Production, na.rm = T))

    Debt <- Debt %>%
      ungroup() %>%
      select(investor_name, portfolio_name, ald_sector, Production) %>%
      group_by(investor_name, portfolio_name, ald_sector) %>%
      summarise(ald_sectorProd = sum(Production, na.rm = T))

    Debt$ald_sector <- ifelse(Debt$ald_sector == "Oil&Gas", "OilGas", Debt$ald_sector)
    if (nrow(Debt) > 0) {
      Debt$Indicator <- ifelse(Debt$ald_sectorProd > 0, TRUE, FALSE)
      Debt$ald_sectorProd <- NULL
      Debt$ald_sector <- paste0(Debt$ald_sector, ".CB")
      Debt <- Debt %>% spread(value = Indicator, key = ald_sector)
      Debt <<- as.data.frame(Debt)

      HasCoalCapCB <<- "CoalCap" %in% DebtTech$technology
      HasRenCapCB <<- "RenewableCap" %in% DebtTech$technology
      # Debt$Indicator <<- ifelse(Debt$  > 0,1,0)

      has_debt <<- TRUE
    } else {
      has_debt <<- FALSE
    }
  }

  # missingcols <- rbind(paste0(ald_sectors,".EQ"),paste0(ald_sectors,".CB"))

  if (has_debt == TRUE & has_equity == TRUE) {
    test_list <- base::merge(Equity, Debt, by = ID.COLS)
    test_list[is.na(test_list)] <- FALSE
    colnames(test_list) <- gsub("\\.x", ".EQ", colnames(test_list))
    colnames(test_list) <- gsub("\\.y", ".CB", colnames(test_list))
  } else if (has_debt == FALSE & has_equity == TRUE) {
    test_list <- Equity
    # test_list$has_debt <- FALSE
  } else if (has_debt == TRUE & has_equity == FALSE) {
    test_list <- Debt
    # test_list$has_equity <- FALSE
  } else {
    test_list <- NA
  }

  eqcols <- paste0(sector_list, ".EQ")
  cbcols <- paste0(sector_list, ".CB")

  missingeqcols <- setdiff(eqcols, colnames(test_list))
  missingcbcols <- setdiff(cbcols, colnames(test_list))
  test_list[missingeqcols] <- FALSE
  test_list[missingcbcols] <- FALSE


  if (is.data.frame(test_list)) {
    test_list <- test_list %>%
      filter(portfolio_name == portfolio_name_select)
  }




  return(test_list)
}

define_test_variables <- function(test_list) {
  PortSummary <<- test_list[1, ]

  # TestType <<- test_list[1,"Type"]
  portfolio_name_selectLong <<- test_list[1, "portfolio_name_select"]
  investor_name_selectLong <<- test_list[1, "investor_name_select"]
  investor_name_select <<- gsub(" ", "", test_list[1, "investor_name_select"])
  portfolio_name_select <<- gsub("[[:punct:]]", "", test_list[1, "portfolio_name"])
  # portfolio_name_select <<- gsub(" ", "", test_list[1,"PortName"]) # KH: Adjusted this as i assume it wasn?t planned to overwrite portfolio_name_select
  portfolio_name_select <<- gsub(" ", "", portfolio_name_select)
  has_equity <<- test_list[1, "has_equity"]
  has_debt <<- test_list[1, "has_debt"]
}

graph_values <- function() {
  textfont <<- "Calibri"
  windowsFonts(Calibri = windowsFont(textfont))

  # orangeish
  RenewablesColour <<- "#feedde"
  HydroColour <<- "#fdbe85"
  NuclearColour <<- "#fd8d3c"
  GasCapColour <<- "#e6550d"
  CoalCapColour <<- "#a63603"

  # purpleish
  ElectricColour <<- "#efedf5"
  HybridColour <<- "#bcbddc"
  ICEColour <<- "#756bb1"

  # goldish
  GasProdColour <<- "#D9DDD4" # "#F5F5F5" #D9DDD4
  OilProdColour <<- "#BEBCAE" # "#BEA07B" #BEBCAE
  CoalProdColour <<- "#8B7E66" # "#8C510A" #8B7E66

  # blueish
  FreightColor <<- "#6ba7e3"
  PassengerColor <<- "#d6e7f7"

  #
  GColor <<- "#D73027"
  FColor <<- "#FC8D59"
  EColor <<- "#FEE08B"
  DColor <<- "#FFFFBF"
  CColor <<- "#D9EF8B"
  Bcolor <<- "#91CF60"
  AColor <<- "#1A9850"

  # map
  MAXRenewablesCap <<- "#a75105"
  MAXHydroCap <<- "#9b4b03"
  MAXNuclearCap <<- "#6b2e01"
  MAXGasCap <<- "#260e02"
  MAXCoalCap <<- "#290e01"
  MAXElectric <<- "#6b5aa3"
  MAXHybrid <<- "#464987"
  MAXICE <<- "#3d3667"
  MAXGas <<- "#737f65"
  MAXOil <<- "#595647"
  MAXCoal <<- "#423b30"

  MINRenewablesCap <<- "#fde0c5"
  MINHydroCap <<- "#fdcb9e"
  MINNuclearCap <<- "#feb998"
  MINGasCap <<- "#f5834b"
  MINCoalCap <<- "#d84604"
  MINElectric <<- "#e0ddec"
  MINHybrid <<- "#bcbddc"
  MINICE <<- "#a49dcc"
  MINGas <<- "#D9DDD4"
  MINOil <<- "#d5d3ca"
  MINCoal <<- "#b9af9e"

  # sector
  energy <<- "#8B7E66"
  pow <<- "#a63603"
  trans <<- "#756bb1"
  othr <<- "#9793c6"
  notrelevant <<- "grey"
  shipping <<- "#0F1e82" #
  material <<- "#e46c00" #


  # trajectory
  area_6 <<- "#e07b73"
  area_4_6 <<- "#fde291"
  area_2_4 <<- "#FFFFCC"
  area_2 <<- "#c3d69b"
  area_1 <<- "#9cab7c"
  eq_port <<- "#1056ff"

  peer_group <<- "black"

  eq_line <<- "#a63603" # same as CoalCapColor
  cb_line <<- "#756bb1" # same as ICEColour
  bv_line <<- "#265B9B"

  # side-by-side techshare
  Port <<- "#265B9B"
  Benchmark <<- "#D9D9D9"
  # moodys
  HighRisk <<- area_6
  MedRisk <<- area_4_6
  LowRisk <<- area_2_4

  # physical risk
  pal_stacked_riskWS <<- c("#D73027", area_6, area_2, area_2_4) # water stress charts
  pal_stacked_risks <<- c(area_6, area_2_4, area_2) # flood and wildfire charts

  # text size
  textsize <<- 8.5
  textcolor <<- "#3D3D3C"
  AxisColour <<- "#3D3D3C"

  YourportColour <<- "#265b9b" # "#2e4b6e"  #"#17224D"
  IndexColour <<- "grey85"
  Tar2DColourBar <<- "#b3de69"
  Tar2DColour <<- "#a1c75e"
  goodexpColour <<- "#1F891F"
  badexpColour <<- "#ed1c24" # "#fb8072"
  ReqCapColour <<- "grey55"
  CurrCapColour <<- "grey75"

  technology_order <<- c(
    "Gas", "Oil", "Coal",
    "RenewablesCap", "HydroCap", "NuclearCap", "GasCap", "CoalCap",
    "Electric", "Hybrid", "ICE",
    "Freight", "Passenger",
    "A", "B", "C", "D", "E", "F", "G"
  )
  sector_order_short <<- c("FossilFuels", "Power", "Automotive")
  technology_order_short <<- c(
    "Gas", "Oil", "Coal",
    "RenewablesCap", "HydroCap", "NuclearCap", "GasCap", "CoalCap",
    "Electric", "Hybrid", "ICE"
  )

  goodtechs <<- c("RenewablesCap", "HydroCap", "NuclearCap", "Electric", "Hybrid")
  badtechs <<- c("GasCap", "CoalCap", "ICE", "Gas", "Oil", "Coal")

  ColourPalette <<- data.frame(
    ald_sector = c(
      "Fossil Fuels", "Fossil Fuels", "Fossil Fuels",
      "Power", "Power", "Power", "Power", "Power",
      "Automotive", "Automotive", "Automotive",
      "Aviation", "Aviation",
      "Shipping", "Shipping", "Shipping", "Shipping", "Shipping", "Shipping", "Shipping"
    ),
    technology = technology_order,
    Colours = c(
      GasProdColour, OilProdColour, CoalProdColour,
      RenewablesColour, HydroColour, NuclearColour, GasCapColour, CoalCapColour,
      ElectricColour, HybridColour, ICEColour,
      FreightColor, PassengerColor,
      AColor, Bcolor, CColor, DColor, EColor, FColor, GColor
    )
  )


  ColourPalette$technology <- factor(ColourPalette$technology, levels = technology_order)
  ColourPalette <- arrange(ColourPalette, technology)

  linesize <<- 1
  ppi <<- 600
}

graph_name <- function(plotnumber, ParameterFile, explicit_filename = "") {
  namelist <- ParameterFile[, !names(ParameterFile) %in% c("portfolio_name_select", "investor_name_select")]
  namelist[is.na(namelist)] <- ""
  namelist <- lapply(1:length(namelist), function(x) paste0(toupper(substr(as.character(namelist[[x]]), 1, 3)), "_"))
  namelist <- namelist[1:6]
  namelist <- c(namelist, start_year)

  graphname <- do.call(paste0, namelist)
  graphname <- paste0(graphname, ".png")

  if (plotnumber == "00") {
    graphname <- gsub(".png", "", graphname)
  } else if (plotnumber != "99") {
    graphname <- paste0(report_path, plotnumber, "_", explicit_filename, graphname)
  } else {
    graphname <- paste0(GRAPH.PATH, graphname)
  }



  return(graphname)
}

green_brown <- function(Tech) {
  GreenTechs <- c("Electric", "Hybrid", "RenewablesCap", "HydroCap", "NuclearCap")

  if (Tech %in% GreenTechs) {
    TechIs <- "green"
  } else {
    TechIs <- "brown"
  }

  return(TechIs)
}

set_ald_sector <- function(Tech) {
  ald_sector <- NA
  ald_sector <- ifelse(grepl("Cap", Tech), "Power", ald_sector)
  ald_sector <- ifelse(Tech %in% c("Electric", "ICE", "Hybrid", "FuelCell"), "Automotive", ald_sector)
  ald_sector <- ifelse(Tech %in% c("Oil", "Gas"), "Oil&Gas", ald_sector)
  ald_sector <- ifelse(Tech == "Coal", "Coal", ald_sector)
  ald_sector
}

translate_labels <- function(Language) {
  Translations <- read_excel(paste0("data/TranslationFileUTF8.xlsx"))

  Translations <- Translations %>%
    select(TextLabel, all_of(Language)) %>%
    as.data.frame()

  for (t in 1:nrow(Translations)) {
    assign(x = Translations[t, "TextLabel"], value = Translations[t, Language], envir = .GlobalEnv)
  }
}

ReportFigures <- function(explicit_filenames = F) {
  explicit_filename <- ""
  # Introduction
  if (!explicit_filenames == F) {
    explicit_filename <- "_scope_of_analysis_"
  }
  ScopeOfAnalysis("01", explicit_filename = explicit_filename) #

  if (!explicit_filenames == F) {
    explicit_filename <- "_climate_relevant_sectors_"
  }
  PercentageOfPortfolioAssessed("02", explicit_filename = explicit_filename) #

  ScenarioGeographyChoose <- "Global"
  if (!explicit_filenames == F) {
    explicit_filename <- "_current_exposure_bonds_"
  }
  CarstenMetricChart("03", "CB", explicit_filename = explicit_filename) #

  if (!explicit_filenames == F) {
    explicit_filename <- "_current_expousre_equity_"
  }
  CarstenMetricChart("04", "EQ", explicit_filename = explicit_filename) #

  ScenarioGeographyChoose <- "GlobalAggregate"

  if (!explicit_filenames == F) {
    explicit_filename <- "_future_tech_mix_bonds_"
  }
  TechnologyExposure("05", "CB", "All", start_year + 5, explicit_filename = explicit_filename) #

  if (has_debt) {
    # 5 year Trajectory

    if (!explicit_filenames == F) {
      explicit_filename <- "_alignment_power_coal_bonds_"
    }
    FiveYearGrowthTrend("07", "CB", "CoalCap", LegendOn = F, explicit_filename = explicit_filename)
    if (!explicit_filenames == F) {
      explicit_filename <- "_alignment_power_renewables_bonds_"
    }
    FiveYearGrowthTrend("08", "CB", "RenewablesCap", LegendOn = F, explicit_filename = explicit_filename)
    if (!explicit_filenames == F) {
      explicit_filename <- "_alignment_power_coal_bonds_"
    }
    FiveYearGrowthTrend("09", "CB", "GasCap", LegendOn = F, explicit_filename = explicit_filename)
    if (!explicit_filenames == F) {
      explicit_filename <- "_alignment_power_hydro_bonds_"
    }
    FiveYearGrowthTrend("10", "CB", "HydroCap", LegendOn = F, explicit_filename = explicit_filename)
    if (!explicit_filenames == F) {
      explicit_filename <- "_alignment_fossil_oil_bonds_"
    }
    FiveYearGrowthTrend("11", "CB", "Oil", LegendOn = F, explicit_filename = explicit_filename)
    if (!explicit_filenames == F) {
      explicit_filename <- "_alignment_fossil_gas_bonds_"
    }
    FiveYearGrowthTrend("12", "CB", "Gas", LegendOn = F, explicit_filename = explicit_filename)
    if (!explicit_filenames == F) {
      explicit_filename <- "_alignment_fossil_coal_bonds_"
    }
    FiveYearGrowthTrend("13", "CB", "Coal", LegendOn = F, explicit_filename = explicit_filename)
    if (!explicit_filenames == F) {
      explicit_filename <- "_alignment_auto_ice_bonds_"
    }
    FiveYearGrowthTrend("14", "CB", "ICE", LegendOn = F, explicit_filename = explicit_filename)
    if (!explicit_filenames == F) {
      explicit_filename <- "_alignment_auto_electric_bonds_"
    }
    FiveYearGrowthTrend("15", "CB", "Electric", LegendOn = F, explicit_filename = explicit_filename)
    if (!explicit_filenames == F) {
      explicit_filename <- "_alignment_auto_hybrid_bonds_"
    }
    FiveYearGrowthTrend("16", "CB", "Hybrid", LegendOn = F, explicit_filename = explicit_filename)
  }

  if (!explicit_filenames == F) {
    explicit_filename <- "_future_tech_mix_equity_"
  }
  TechnologyExposure("06", "EQ", "All", start_year + 5, explicit_filename = explicit_filename) #

  if (has_equity) {

    # 5 year Trajectory
    if (!explicit_filenames == F) {
      explicit_filename <- "_alignment_power_coal_equity_"
    }
    FiveYearGrowthTrend("17", "EQ", "CoalCap", LegendOn = F, explicit_filename = explicit_filename)
    if (!explicit_filenames == F) {
      explicit_filename <- "_alignment_power_renewables_equity_"
    }
    FiveYearGrowthTrend("18", "EQ", "RenewablesCap", LegendOn = F, explicit_filename = explicit_filename)
    if (!explicit_filenames == F) {
      explicit_filename <- "_alignment_power_hydro_equity_"
    }
    FiveYearGrowthTrend("19", "EQ", "GasCap", LegendOn = F, explicit_filename = explicit_filename)
    if (!explicit_filenames == F) {
      explicit_filename <- "_alignment_power_hydro_equity_"
    }
    FiveYearGrowthTrend("20", "EQ", "HydroCap", LegendOn = F, explicit_filename = explicit_filename)
    if (!explicit_filenames == F) {
      explicit_filename <- "_alignment_fossil_oil_equity_"
    }
    FiveYearGrowthTrend("21", "EQ", "Oil", LegendOn = F, explicit_filename = explicit_filename)
    if (!explicit_filenames == F) {
      explicit_filename <- "_alignment_fossil_gas_equity_"
    }
    FiveYearGrowthTrend("22", "EQ", "Gas", LegendOn = F, explicit_filename = explicit_filename)
    if (!explicit_filenames == F) {
      explicit_filename <- "_alignment_fossil_coal_equity_"
    }
    FiveYearGrowthTrend("23", "EQ", "Coal", LegendOn = F, explicit_filename = explicit_filename)
    if (!explicit_filenames == F) {
      explicit_filename <- "_alignment_auto_ice_equity_"
    }
    FiveYearGrowthTrend("24", "EQ", "ICE", LegendOn = F, explicit_filename = explicit_filename)
    if (!explicit_filenames == F) {
      explicit_filename <- "_alignment_auto_electric_equity_"
    }
    FiveYearGrowthTrend("25", "EQ", "Electric", LegendOn = F, explicit_filename = explicit_filename)
    if (!explicit_filenames == F) {
      explicit_filename <- "_alignment_auto_hybrid_equity_"
    }
    FiveYearGrowthTrend("26", "EQ", "Hybrid", LegendOn = F, explicit_filename = explicit_filename)
  }

  if (IncOtherSectors == T) {
    if (!explicit_filenames == F) {
      explicit_filename <- "_emissions_cement_"
    }
    CO2IntensityTrend("30", "Cement", LegendOn = F, explicit_filename = explicit_filename)
    if (!explicit_filenames == F) {
      explicit_filename <- "_emissions_steel_"
    }
    CO2IntensityTrend("31", "Steel", LegendOn = F, explicit_filename = explicit_filename)
    if (!explicit_filenames == F) {
      explicit_filename <- "_emissions_aviation_"
    }
    CO2IntensityTrend("32", "Aviation", LegendOn = F, explicit_filename = explicit_filename)
    if (!explicit_filenames == F) {
      explicit_filename <- "_emissions_shipping_"
    }
    ShippingChart("33", "All", start_year + 5, explicit_filename = explicit_filename) # All
  }

  if (has_map) {
    if (!explicit_filenames == F) {
      explicit_filename <- "_map_coal_bonds_"
    }
    MapChart("49", "CB", "Coal", start_year + 5, explicit_filename = explicit_filename)
    if (!explicit_filenames == F) {
      explicit_filename <- "_map_coal_equity_"
    }
    MapChart("50", "EQ", "Coal", start_year + 5, explicit_filename = explicit_filename)
  }

  if (has_debt) {
    if (!explicit_filenames == F) {
      explicit_filename <- "_companies_power_bonds_"
    }
    CompanyInformation("40", no_companies, "CB", "Power", explicit_filename = explicit_filename)
    if (!explicit_filenames == F) {
      explicit_filename <- "_companies_auto_bonds_"
    }
    CompanyInformation("41", no_companies, "CB", "Automotive", explicit_filename = explicit_filename)
    if (!explicit_filenames == F) {
      explicit_filename <- "_companies_oil_built_out_bonds_"
    }
    OilGasBuildOut("42", no_companies, "CB", explicit_filename = explicit_filename)
    if (!explicit_filenames == F) {
      explicit_filename <- "_companies_oil_share_bonds_"
    }
    OilShare("43", no_companies, "CB", explicit_filename = explicit_filename)

    # if (IncCoalRetirement){
    #   CoalRetirementChart("62",no_companies,"CB")
    #   RenewableAdditionsChart("64",no_companies,"CB")
    # }
  }

  if (has_equity) {
    if (IncPeersChart) {
      PeerComparison(81, chart_type = "EQ")
    }
    if (!explicit_filenames == F) {
      explicit_filename <- "_companies_power_equity_"
    }
    CompanyInformation("44", no_companies, "EQ", "Power", explicit_filename = explicit_filename)
    if (!explicit_filenames == F) {
      explicit_filename <- "_companies_auto_equity_"
    }
    CompanyInformation("45", no_companies, "EQ", "Automotive", explicit_filename = explicit_filename)
    if (!explicit_filenames == F) {
      explicit_filename <- "_companies_oil_built_out_equity_"
    }
    OilGasBuildOut("46", no_companies, "EQ", explicit_filename = explicit_filename)
    if (!explicit_filenames == F) {
      explicit_filename <- "_companies_oil_share_equity_"
    }
    OilShare("47", no_companies, "EQ", explicit_filename = explicit_filename)

    # CarbonBudget("48", no_companies, "EQ")

    # if (IncCoalRetirement){
    #   CoalRetirementChart("63",no_companies,"EQ")
    #   RenewableAdditionsChart("65",no_companies,"EQ")
    # }
  }
}

##############
### Charts ###
##############

PercentageOfPortfolioAssessed <- function(plotnumber, explicit_filename = "") {
  over <- SectorDataAnalysis()

  over <- over[over$financial_sector != "Not Included", ]
  # over$financial_sector <- factor(over$financial_sector, levels = c("Fossil Fuels","Power", "Automotive",
  #                                                     "Cement & Steel","Aviation & Shipping"), ordered=TRUE)

  over <- as.data.frame(over)

  if (data_check(over)) {
    over$financial_sector <- if_else(over$financial_sector == "Cement&Steel", "Cement & Steel", over$financial_sector)
    over <- over %>% filter(!is.na(financial_sector))

    ## "steelblue" color below should be changed to whatever our Portfolio color is
    over1 <- subset(over, portfolio_name == portfolio_name_select & asset_type %in% c("Equity", "Bonds"))

    over1 <- over1 %>%
      group_by(investor_name, portfolio_name, asset_type, financial_sector) %>%
      summarise(ValueUSD = sum(valid_value_usd))


    over1$asset_type <- gsub("Bonds", BondReference, over1$asset_type)
    over1$asset_type <- gsub("Equity", "Equity", over1$asset_type)
    over1$asset_type <- factor(over1$asset_type, levels = c("Equity", BondReference))

    # if(BondReference == "Corporate bond") {
    over1$asset_type <- dplyr::recode(over1$asset_type, "Bond" = wrap_labels(BondsTitle, 10), Equity = EquityTitle) #
    # } else {
    # over1$asset_type <- dplyr::recode(over1$asset_type,  "Other Title" = " ", Equity = EquityTitle)  #
    # }

    over1$financial_sector <- factor(over1$financial_sector, levels = c(
      "Aviation & Shipping", "Cement & Steel",
      "Automotive", "Power", "Fossil Fuels"
    ), ordered = TRUE)
    over1$financial_sector <- dplyr::recode(over1$financial_sector,
      "Fossil Fuels" = wrap_labels(S_FossilFuels, 15),
      "Power" = wrap_labels(S_Power, 15),
      "Automotive" = wrap_labels(S_Automotive, 15), #
      "Cement & Steel" = wrap_labels(S_CementSteel, 15),
      "Aviation & Shipping" = wrap_labels(S_AviationShipping, 15)
    ) #

    over_horizontal_perc <- over1 %>%
      group_by(asset_type) %>%
      mutate(percfinancial_sector = ValueUSD / sum(ValueUSD))

    # ymax<-max(aggregate(over1["ValueUSD"],by=over1["asset_type"],FUN=sum)$ValueUSD)

    ## version horizontal percentage
    plot <- ggplot(over_horizontal_perc, aes(x = asset_type, y = percfinancial_sector, fill = financial_sector)) +
      geom_bar(position = "stack", stat = "identity", width = 0.9) +
      coord_flip() +
      scale_fill_manual(name = "", values = c(shipping, pow, trans, material, energy), drop = FALSE) + #
      # scale_x_discrete(name="Asset Type",drop=F, labels=c(BondsTitle, EquityTitle)) + #
      scale_y_continuous(name = SectorShare, labels = percent, expand = c(0, 0)) +
      guides(fill = guide_legend(nrow = 2, reverse = TRUE)) +
      theme_barcharts() +
      theme(
        plot.title = element_text(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size = textsize),
        legend.spacing.y = unit(-22, "pt"),
        legend.box.margin = unit(c(-5, 40, -10, 10), "pt"),
        axis.line.x = element_blank(),
        axis.line.y = element_line(size = 0.5),
        axis.text.x = element_text(colour = textcolor, size = 11),
        axis.text.y = element_text(colour = textcolor, size = 11),
        axis.title.x = element_text(size = 11, margin = margin(3, 0, 0, 0, "pt")),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_line(colour = "#989898", size = 0.2),
        plot.margin = unit(c(-2, 20, 10, 2), "pt")
      ) +
      ggtitle("\n")
  } else {
    label <- Therearenoclimaterelevantequityorbondholdingsintheportfolios #

    plot <- no_chart(label)
  }

  ggsave(plot, filename = graph_name(plotnumber, ParameterFile, explicit_filename = explicit_filename), bg = "white", height = 2.3, width = 4.5, dpi = ppi) # linewidth_in*.9
}

TechnologyExposure <- function(plotnumber, chart_type, sector_to_plot, plot_year, explicit_filename = "") { # PlotYr

  Combin <- data.frame()

  if (chart_type == "EQ" & has_equity == TRUE) {
    Peers <- filter_by_parameters(eq_peers, "EQ")
    Market <- filter_by_parameters(eq_market, "EQ")
    Combin <- filter_by_parameters(EQCombin, "EQ")
  } else if (chart_type == "CB" & has_debt == TRUE) {
    Peers <- filter_by_parameters(cb_peers, "CB", by_equity_market = FALSE)
    Market <- filter_by_parameters(cb_market, "CB", by_equity_market = FALSE)
    Combin <- filter_by_parameters(CBCombin, "CB", by_equity_market = FALSE)
  }
  if (data_check(Combin) == TRUE) {
    PlotChart <- TRUE
  } else {
    PlotChart <- FALSE
  }

  if (PlotChart == TRUE) {

    # print("format and bind data")
    # Remove all portfolios other than Market, Average
    Peers$Type <- "Peers"
    Peers <- unique(Peers)
    Peers <- Peers %>%
      ungroup() %>%
      filter(technology != "OilCap" & year == plot_year) %>% # year == PlotYr &
      select("portfolio_name", "ald_sector", "technology", "Type", "plan_alloc_wt_tech_prod") %>% # "Type
      rename(WtProduction = plan_alloc_wt_tech_prod)

    Peers <- Peers %>%
      complete(
        ald_sector = c("Oil&Gas", "Coal", "Power", "Automotive"), technology = c(technology_order),
        fill = list(WtProduction = 0, Type = "Peers")
      )
    Peer_A <- subset(Peers, ald_sector == "Automotive" & technology %in% c("ICE", "Electric", "Hybrid"))
    Peer_P <- subset(Peers, ald_sector == "Power" & technology %in% c("RenewablesCap", "HydroCap", "NuclearCap", "GasCap", "CoalCap"))
    Peer_F <- subset(Peers, (ald_sector == "Coal" & technology %in% c("Coal")) | (ald_sector == "Oil&Gas" & technology %in% c("Oil", "Gas")))

    Peers <- bind_rows(Peer_F, Peer_A, Peer_P)
    if (plot_year == start_year + 5) {
      TARGET <- Combin %>%
        ungroup() %>%
        filter(technology != "OilCap" & year == plot_year) %>%
        select("portfolio_name", "ald_sector", "technology", "scen_alloc_wt_tech_prod") %>% # "Type"
        rename(WtProduction = scen_alloc_wt_tech_prod)
      TARGET$Type <- "Target"

      TARGET <- TARGET %>%
        complete(
          ald_sector = c("Oil&Gas", "Coal", "Power", "Automotive"), technology = c(technology_order),
          fill = list(WtProduction = 0, Type = "Target")
        )
      TARGET_A <- subset(TARGET, ald_sector == "Automotive" & technology %in% c("ICE", "Electric", "Hybrid"))
      TARGET_P <- subset(TARGET, ald_sector == "Power" & technology %in% c("RenewablesCap", "HydroCap", "NuclearCap", "GasCap", "CoalCap"))
      TARGET_F <- subset(TARGET, (ald_sector == "Coal" & technology %in% c("Coal")) | (ald_sector == "Oil&Gas" & technology %in% c("Oil", "Gas")))

      TARGET <- bind_rows(TARGET_F, TARGET_A, TARGET_P)
    }

    Combin$Type <- "Portfolio"
    Production <- Combin %>%
      ungroup() %>%
      filter(technology != "OilCap" & year == plot_year) %>% # year == PlotYr &
      select("portfolio_name", "ald_sector", "technology", "plan_alloc_wt_tech_prod", "Type") %>% # "Type
      rename(WtProduction = plan_alloc_wt_tech_prod)

    Market$Type <- "Benchmark"
    Market <- Market %>%
      ungroup() %>%
      filter(technology != "OilCap" & year == plot_year) %>% # year == PlotYr & Type =="Portfolio") %>%
      select("portfolio_name", "ald_sector", "technology", "scen_alloc_wt_tech_prod", "Type") %>% # "Type"
      rename(WtProduction = scen_alloc_wt_tech_prod)

    Market <- Market %>%
      complete(
        ald_sector = c("Oil&Gas", "Coal", "Power", "Automotive"), technology = c(technology_order),
        fill = list(WtProduction = 0, Type = "Benchmark")
      )
    Market_A <- subset(Market, ald_sector == "Automotive" & technology %in% c("ICE", "Electric", "Hybrid"))
    Market_P <- subset(Market, ald_sector == "Power" & technology %in% c("RenewablesCap", "HydroCap", "NuclearCap", "GasCap", "CoalCap"))
    Market_F <- subset(Market, (ald_sector == "Coal" & technology %in% c("Coal")) | (ald_sector == "Oil&Gas" & technology %in% c("Oil", "Gas")))

    Market <- bind_rows(Market_F, Market_A, Market_P)

    # Filter and select
    # print("Filter and select data")
    if (plot_year == start_year + 5) {
      Production <- bind_rows(Production, Peers, TARGET, Market)
    } else if (plot_year == start_year) {
      Production <- bind_rows(Production, Peers, Market)
    }

    Production$ald_sector <- factor(Production$ald_sector, levels = c("Oil&Gas", "Coal", "Power", "Automotive"))
    levels(Production$ald_sector)[levels(Production$ald_sector) == "Coal"] <- "Fossil Fuels"
    levels(Production$ald_sector)[levels(Production$ald_sector) == "Oil&Gas"] <- "Fossil Fuels"
    Production$ald_sector <- factor(Production$ald_sector, levels = c("Fossil Fuels", "Power", "Automotive"))

    # Aggregate and rename CarstenMetric_Port
    ID.COLS <- c("ald_sector", "technology", "Type")
    Production <- Production %>% gather(key = Metric, value = Value, "WtProduction")
    Production <- aggregate(Production["Value"], by = Production[c(ID.COLS)], FUN = sum)
    # Created an average for the peers (or even just use fill!)

    # Transfer coal to gigajoule for tech share charts only!!!
    Production$Value[Production$technology == "Coal"] <- Production$Value[Production$technology == "Coal"] * 29307600 / 365.25 / 1e6

    # , Production$Type != "Market"

    if (sector_to_plot != "All") {
      technologyorder <- technology_order
      colours <- as.vector(ColourPalette[["Colours"]])
      names(colours) <- technologyorder

      # labels <-  gsub("Cap","",technologyorder)
      labels <- dplyr::recode(technologyorder,
        Gas = T_GasProd, Oil = T_OilProd, Coal = T_CoalProd, #
        RenewablesCap = T_Renewables, HydroCap = T_HydroCap, NuclearCap = T_NuclearCap, GasCap = T_GasCap, CoalCap = T_CoalCap, #
        Electric = x_Electric, Hybrid = x_Hybrid, ICE = x_ICE, "Freight Passenger" = FreightPassenger
      )
      names(labels) <- technologyorder
    } else if (sector_to_plot == "All") {
      technologyorder <- technology_order_short
      colours <- as.vector(ColourPalette[["Colours"]])[1:11]
      names(colours) <- technologyorder
      # labels <-  gsub("Cap","",technologyorder)
      labels <- dplyr::recode(technologyorder,
        Gas = T_GasProd, Oil = T_OilProd, Coal = wrap_labels(paste0(T_CoalProd, "       "), 10), #
        RenewablesCap = wrap_labels(T_Renewables, 10), HydroCap = wrap_labels(T_HydroCap, 15), NuclearCap = wrap_labels(T_NuclearCap, 15), GasCap = wrap_labels(T_GasCap, 15), CoalCap = wrap_labels(T_CoalCap, 15), #
        Electric = wrap_labels(x_Electric, 10), Hybrid = wrap_labels(x_Hybrid, 10), ICE = wrap_labels(x_ICE, 10), "Freight Passenger" = FreightPassenger
      )
      names(labels) <- technologyorder
    }


    Production$technology <- factor(Production$technology, levels = technologyorder)
    Production$ald_sector <- factor(Production$ald_sector, levels = c("Fossil Fuels", "Power", "Automotive", "Aviation", "Shipping"))

    Production$Type <- wrap_labels(Production$Type, 20)
    if (plot_year == start_year + 5) {
      Production$Type <- factor(Production$Type, levels = c("Portfolio", "Peers", "Target", "Benchmark"))
      xlabels <- c(
        Portf, wrap_labels(x_Peers, 10), ifelse(Language %in% c("FR", "ES"), paste(Portf, "\n", AlignedFEM), paste(Aligned, "\n", Portf)), #
        ifelse(Language %in% c("FR", "ES"), paste(AlignMarket, "\n", AlignedMAS), paste(Aligned, "\n", AlignMarket))
      ) #
    } else if (plot_year == start_year) {
      Production$Type <- factor(Production$Type, levels = c("Portfolio", "Peers", "Benchmark"))
      xlabels <- c(Portf, wrap_labels(x_Peers, 9), ifelse(Language %in% c("FR", "ES"), paste(AlignMarket, "\n", Aligned), paste(Aligned, "\n", AlignMarket))) #
    }
    titles <- c("Fossil Fuel Production", "Power Capacity", "Automotive Production", "Aviation Production", "Shipping Production")
    names(titles) <- c("Fossil Fuels", "Power", "Automotive", "Aviation", "Shipping")


    Production <- subset(Production, select = c("Type", "ald_sector", "technology", "Value"))

    plottheme <- stacked_bar_chart(Production, colours, labels) +
      ylab(ShareofSectorProduction) +
      scale_x_discrete(labels = xlabels) +
      guides(fill = guide_legend(
        keywidth = 0.1,
        keyheight = 0.1,
        default.unit = "inch"
      )) +
      theme(
        plot.title = element_text(hjust = 0.5, colour = textcolor, size = 11, margin = unit(c(0, 0, 1, 0), "lines")),
        legend.position = "bottom",
        legend.box.margin = margin(-15, 0, 0, 0, unit = "pt"),
        legend.title = element_blank(),
        legend.text = element_text(size = 6.5), # , margin = margin(l = -0.5, r = -.5, unit = "pt")
        axis.text.x = element_text(size = 6.5),
        axis.text.y = element_text(size = 6.5)
      )


    if (sector_to_plot %in% c("Automotive", "Power", "Fossil Fuels", "Aviation", "Shipping")) {
      # print("plotting")
      dat <- subset(Production, ald_sector == sector_to_plot)
      cmd <- plottheme %+% dat +
        xlab("")

      # print(p1)

      # if (sector_to_plot == "Fossil Fuels"){
      #   sector_to_plot <- "FossilFuels"
      # }

      n <- 4
    } else if (sector_to_plot %in% c("All", "")) {
      dat <- subset(Production, ald_sector == "Automotive")
      if (nrow(subset(dat, Type == "Portfolio")) > 0) {
        p1 <- plottheme %+% dat +
          ggtitle(AutomotiveProduction) #
      } else {
        dat <- rbind(dat, c(
          "Type" = "Portfolio",
          "ald_sector" = "Automotive",
          "technology" = "ICE",
          "Value" = 0
        ))
        dat$Value <- as.numeric(dat$Value)
        p1 <- plottheme %+% dat +
          ggtitle(AutomotiveProduction) + #
          geom_text(
            data = subset(dat, Type == "Portfolio"),
            aes(Type, y = .5, angle = 90, label = NoAutomotiveProduction), size = 2.5
          ) #
      }


      dat <- subset(Production, ald_sector == "Fossil Fuels")
      if (nrow(subset(dat, Type == "Portfolio")) > 0) {
        p2 <- plottheme %+% dat +
          ggtitle(FossilFuelsProduction) #
      } else {
        dat <- rbind(dat, c(
          "Type" = "Portfolio",
          "ald_sector" = "Fossil Fuels",
          "technology" = "OilProd",
          "Value" = 0
        ))
        dat$Value <- as.numeric(dat$Value)
        p2 <- plottheme %+% dat +
          ggtitle(FossilFuelsProduction) + #
          geom_text(
            data = subset(dat, Type == "Portfolio"),
            aes(Type, y = 0.5, angle = 90, label = NoFossilFuelsProduction), size = 2.5
          ) #
      }

      dat <- subset(Production, ald_sector == "Power")
      if (nrow(subset(dat, Type == "Portfolio")) > 0) {
        p3 <- plottheme %+% dat +
          ggtitle(C_Power) #
      } else {
        dat <- rbind(dat, c(
          "Type" = "Portfolio",
          "ald_sector" = "Power",
          "technology" = "CoalCap",
          "Value" = 0
        ))
        dat$Value <- as.numeric(dat$Value)
        p3 <- plottheme %+% dat +
          ggtitle(C_Power) + #
          geom_text(
            data = subset(dat, Type == "Portfolio"),
            aes(Type, y = .5, angle = 90, label = NoC_Power), size = 2.5
          ) #
      }

      # print("plotting")
      n <- 8
      cmd <- arrangeGrob(p2,
        p3,
        p1,
        nrow = 1
      )
      # p3+theme(axis.text.y =  element_blank(), axis.title.y =  element_blank()),
      # p1+theme(axis.text.y =  element_blank(), axis.title.y =  element_blank()), nrow=1)
      # p3+theme(axis.text.y = element_text(color="white"), axis.title.y = element_text(color="white")),
      # p1+theme(axis.text.y = element_text(color="white"), axis.title.y = element_text(color="white")), nrow=1)


      dev.off()
    }
  } else {
    Label <- YourPortfoliohasnoproductioninthissectorfortheselectedscenario_geographyandMarket #
    cmd <- no_chart(Label)
    n <- 4
  }

  ggsave(graph_name(plotnumber, ParameterFile, explicit_filename = explicit_filename), # bg = "transparent",
    plot = cmd, height = 3, width = n, dpi = ppi
  )
}

FiveYearGrowthTrend <- function(plotnumber, chart_type, tech_to_plot, LegendOn = FALSE, explicit_filename = "") {
  tech_to_plot <<- tech_to_plot

  GoodBad <- green_brown(tech_to_plot)
  if (GraphType %in% c("", "Report")) {
    LegendOn <- FALSE
  } else {
    LegendOn <- TRUE
  }

  filternames <- c("Listed Market", "Bond Universe", "ListedEquity", "MetaPort", meta_investor_name)
  PortNames <- portfolio_name_select

  ### Equity PRODUCTION
  if (chart_type == "EQ") {
    ALD_Market <- filter_by_parameters(eq_market, "EQ", byscenario = TRUE) %>% distinct()
    ALD <- filter_by_parameters(EQCombin, "EQ", byscenario = FALSE) %>% filter(ald_sector != "Other")
    if (data_check(ALD) == TRUE) {
      ALD$asset_type <- "Equity"
      ALD_Market <- ALD_Market %>%
        filter(portfolio_name == eq_market_ref)

      if (sum(ALD$plan_alloc_wt_tech_prod) > 0) {
        PlotChart <- TRUE
      } else {
        PlotChart <- FALSE
      }
    }
  }

  if (chart_type == "CB") {
    # ScenarioGeographyChoose <- "Global"
    ALD_Market <- filter_by_parameters(cb_market, "CB", byscenario = TRUE, by_equity_market = FALSE)
    # ScenarioGeographyChoose <- "GlobalAggregate"

    ALD <- filter_by_parameters(CBCombin, "CB", byscenario = FALSE, by_equity_market = FALSE)
    if (data_check(ALD) == TRUE) {
      ALD$asset_type <- "Bonds"
      if (sum(ALD$plan_alloc_wt_tech_prod, na.rm = T) > 0) {
        PlotChart <- TRUE
      } else {
        PlotChart <- FALSE
      }
    }
  }

  if (PlotChart == TRUE) {
    ALD$plan_alloc_wt_tech_prod <- ifelse(ALD$technology == "Oil", ALD$plan_alloc_wt_tech_prod * (1 / 6.12), ALD$plan_alloc_wt_tech_prod)
    ALD$plan_alloc_wt_tech_prod <- ifelse(ALD$technology == "Gas", ALD$plan_alloc_wt_tech_prod * (1 / 0.0372), ALD$plan_alloc_wt_tech_prod)

    ALD$scen_alloc_wt_tech_prod <- ifelse(ALD$technology == "Oil", ALD$scen_alloc_wt_tech_prod * (1 / 6.12), ALD$scen_alloc_wt_tech_prod)
    ALD$scen_alloc_wt_tech_prod <- ifelse(ALD$technology == "Gas", ALD$scen_alloc_wt_tech_prod * (1 / 0.0372), ALD$scen_alloc_wt_tech_prod)

    if (data_check(ALD_Market)) {
      ALD_Market$investor_name <- "Market"
      ALD_Market$plan_alloc_wt_tech_prod <- ifelse(ALD_Market$technology == "Oil", ALD_Market$plan_alloc_wt_tech_prod * (1 / 6.12), ALD_Market$plan_alloc_wt_tech_prod)
      ALD_Market$plan_alloc_wt_tech_prod <- ifelse(ALD_Market$technology == "Gas", ALD_Market$plan_alloc_wt_tech_prod * (1 / 0.0372), ALD_Market$plan_alloc_wt_tech_prod)
    }

    TotalProduction <- sum(ALD$plan_tech_prod, na.rm = T)

    ALD <- bind_rows(ALD, ALD_Market)
    ALD <- subset(ALD, year >= start_year & year <= start_year + 5)

    ALD <- ALD[ALD$technology == tech_to_plot, ]

    table(ALD$asset_type, useNA = "always")

    ### Separate into CurrentPlans and scenario, get into same column headers
    ALD <- subset(ALD, select = c(investor_name, portfolio_name, ald_sector, year, technology, scenario, plan_alloc_wt_tech_prod, scen_alloc_wt_tech_prod))
    ALD.cp <- ALD %>%
      select(investor_name, portfolio_name, ald_sector, scenario, year, technology, plan_alloc_wt_tech_prod) %>% # Tech.Type,
      distinct() %>%
      rename(Production = plan_alloc_wt_tech_prod) %>%
      filter(
        year %in% seq(start_year, start_year + 5),
        scenario == Scenariochoose
      ) %>%
      mutate(Line.Type = "CurrentPlan")


    ALD.sc <- ALD %>%
      select(investor_name, portfolio_name, scenario, ald_sector, year, technology, scen_alloc_wt_tech_prod) %>% # Tech.Type,
      rename(Production = scen_alloc_wt_tech_prod) %>%
      filter(
        portfolio_name == portfolio_name_select,
        year %in% seq(start_year, start_year + 5),
        scenario %in% iea_scenario_list
      ) %>%
      mutate(Line.Type = "scenario")

    ### Rename the scenarios from Green to Red - so the names are then independent
    ### Set the number of scenarios that are available

    ALD.sc$scenario <- ifelse(ALD.sc$scenario == "NPS", "NPSRTS", as.character(ALD.sc$scenario)) # New policy scenarios
    ALD.sc$scenario <- ifelse(ALD.sc$scenario %in% c("450S", "SDS"), "SDS", as.character(ALD.sc$scenario)) # New policy scenarios
    ALD.sc$scenario <- ifelse(ALD.sc$scenario == "CPSProxy", "CPS", as.character(ALD.sc$scenario))

    no.scenarios <- 0

    if (n_distinct(ALD.sc$scenario) == 3) {
      no.scenarios <- 3
      # ALD.sc$scenario <- ifelse(ALD.sc$scenario == "SDS","Scen2",as.character(ALD.sc$scenario))       # 450S
      # ALD.sc$scenario <- ifelse(ALD.sc$scenario == "NPSRTS","Scen3",as.character(ALD.sc$scenario))    # New policy scenarios
      # ALD.sc$scenario <- ifelse(ALD.sc$scenario == "CPS","Scen4",as.character(ALD.sc$scenario))       # Current policy scenarios
    }

    if (n_distinct(ALD.sc$scenario) == 4) {
      no.scenarios <- 4
      # ALD.sc$scenario <- ifelse(ALD.sc$scenario == "B2DS","Scen1",as.character(ALD.sc$scenario))      # Below 2 degrees
      # ALD.sc$scenario <- ifelse(ALD.sc$scenario == "SDS","Scen2",as.character(ALD.sc$scenario))       # 450S
      # ALD.sc$scenario <- ifelse(ALD.sc$scenario == "NPSRTS","Scen3",as.character(ALD.sc$scenario))    # New policy scenarios
      # ALD.sc$scenario <- ifelse(ALD.sc$scenario == "CPS","Scen4",as.character(ALD.sc$scenario))       # Current policy scenarios
    }

    ALD.sc$scenario <- ifelse(ALD.sc$scenario == "B2DS", "Scen1", as.character(ALD.sc$scenario)) # Below 2 degrees
    ALD.sc$scenario <- ifelse(ALD.sc$scenario == "SDS", "Scen2", as.character(ALD.sc$scenario)) # 450S
    ALD.sc$scenario <- ifelse(ALD.sc$scenario == "NPSRTS", "Scen3", as.character(ALD.sc$scenario)) # New policy scenarios
    ALD.sc$scenario <- ifelse(ALD.sc$scenario == "CPS", "Scen4", as.character(ALD.sc$scenario)) # Current policy scenarios


    ### Remove additional scenarios
    ALD.sc <- as.data.frame(ALD.sc)
    ALD.sc <- ALD.sc[grepl("Scen", ALD.sc[, "scenario"]), ]

    ### Calculate GROWTH
    ALD2 <- bind_rows(ALD.cp, ALD.sc)

    ### Normalisation of market to Portfolio
    ALD.cp <- ALD2 %>% filter(Line.Type == "CurrentPlan")

    var <- ifelse(ALD.cp[which(ALD.cp$portfolio_name == PortNames & ALD.cp$year == start_year & ALD.cp$technology == tech_to_plot), ]$Production == 0, 0,
      ALD.cp[which(ALD.cp$investor_name == "Market" & ALD.cp$year == start_year & ALD.cp$technology == tech_to_plot), ]$Production / ALD.cp[which(ALD.cp$portfolio_name == PortNames & ALD.cp$year == start_year & ALD.cp$technology == tech_to_plot), ]$Production
    )
    # var <- 1
    # ALD.cp[which(ALD.cp$investor_name_select=="Market" & ALD.cp$technology ==tech_to_plot),]$Production<- ifelse(var ==0,0,ALD.cp[which(ALD.cp$investor_name_select=="Market" & ALD.cp$technology ==tech_to_plot),]$Production/var)

    if (data_check(ALD_Market)) {
      if (var == 0) {
        ALD.cp[which(ALD.cp$investor_name == "Market" & ALD.cp$technology == tech_to_plot), ]$Production <- 0
      } else {
        ALD.cp[which(ALD.cp$investor_name == "Market" & ALD.cp$technology == tech_to_plot), ]$Production <- ALD.cp[which(ALD.cp$investor_name == "Market" & ALD.cp$technology == tech_to_plot), ]$Production / var
      }
    }

    if (sum(ALD.cp[which(ALD.cp$investor_name == "Market" & ALD.cp$technology == tech_to_plot), ]$Production) == 0) {
      ALD.cp <- ALD.cp[ALD.cp$investor_name != "Market", ]
    }

    ALD.sc <- ALD2 %>% filter(Line.Type == "scenario")

    ALD2 <- bind_rows(ALD.cp, ALD.sc)

    ylims <- ALD2 %>%
      filter(investor_name != "Market" | Line.Type != "scenario") %>%
      summarise(min = min(Production), max = max(Production))

    ### Units
    unitscaleval <- max(ylims$max, ALD.cp$Production)
    unitscalefactor <- 1
    green.unit <- c(
      "Electric" = Vehicles,
      "RenewablesCap" = "MW",
      "NuclearCap" = "MW",
      "HydroCap" = "MW",
      "Hybrid" = Vehicles
    )

    brown.unit <- c(
      "CoalCap" = "MW",
      "GasCap" = "MW",
      "OilCap" = "MW",
      "Oil" = "bbl",
      "Gas" = "m3",
      "ICE" = Vehicles,
      "Coal" = "t"
    )

    if (unitscaleval < 1e-3) {
      unitscalefactor <- 1e-3
      green.unit <- c(
        "Electric" = paste0("1/1000 x ", Vehicles),
        "RenewablesCap" = "W",
        "NuclearCap" = "W",
        "HydroCap" = "W",
        "Hybrid" = paste0("1/1000 x ", Vehicles)
      )

      brown.unit <- c(
        "CoalCap" = "W",
        "GasCap" = "W",
        "OilCap" = "W",
        "Oil" = paste0("1/1000 x ", "bbl"),
        "Gas" = "L",
        "ICE" = paste0("1/1000 x ", Vehicles),
        "Coal" = "g"
      )
    }
    if (unitscaleval > 1e3) {
      unitscalefactor <- 1e3
      green.unit <- c(
        "Electric" = paste0(thousand, " ", Vehicles),
        "RenewablesCap" = "GW",
        "NuclearCap" = "GW",
        "HydroCap" = "GW",
        "Hybrid" = paste0(thousand, " ", Vehicles)
      )

      brown.unit <- c(
        "CoalCap" = "GW",
        "GasCap" = "GW",
        "OilCap" = "GW",
        "Oil" = "Mbbl",
        "Gas" = paste0(thousand, " m3"),
        "ICE" = paste0(thousand, " ", Vehicles),
        "Coal" = paste0(thousand, " t")
      )
    }
    if (unitscaleval > 1e6) {
      unitscalefactor <- 1e6
      green.unit <- c(
        "Electric" = paste0(million, " ", Vehicles),
        "RenewablesCap" = "TW",
        "NuclearCap" = "TW",
        "HydroCap" = "TW",
        "Hybrid" = paste0(million, " ", Vehicles)
      )

      brown.unit <- c(
        "CoalCap" = "TW",
        "GasCap" = "TW",
        "OilCap" = "TW",
        "Oil" = "MMbbl",
        "Gas" = paste0(million, " m3"),
        "ICE" = paste0(million, " ", Vehicles),
        "Coal" = paste0(million, " t")
      )
    }
    if (unitscaleval > 1e9) {
      unitscalefactor <- 1e9
      green.unit <- c(
        "Electric" = paste0(billion, " ", Vehicles),
        "RenewablesCap" = "PW",
        "NuclearCap" = "PW",
        "HydroCap" = "PW",
        "Hybrid" = paste0(billion, " ", Vehicles)
      )

      brown.unit <- c(
        "CoalCap" = "PW",
        "GasCap" = "PW",
        "Oil" = "Gbbl",
        "Gas" = paste0(billion, " m3"),
        "ICE" = paste0(billion, " ", Vehicles),
        "Coal" = paste0(billion, " t")
      )
    }

    ALD2$Production <- ALD2$Production / unitscalefactor

    ylims <- ALD2 %>%
      filter(investor_name != "Market" | Line.Type != "scenario") %>%
      summarise(min = min(Production), max = max(Production))

    ### GET SCENARIOS INTO RIBBON FORMAT
    ALD.sc <- ALD2 %>% filter(Line.Type == "scenario")
    ALD.cp <- ALD2 %>% filter(Line.Type == "CurrentPlan")

    ALD.sc.wide <- ALD.sc %>%
      ungroup() %>%
      select(investor_name, portfolio_name, scenario, ald_sector, technology, Line.Type, year, Production) %>% # Tech.Type,
      spread(key = scenario, value = Production)


    if (no.scenarios == 3) {
      ALD.sc.wide <- ALD.sc.wide %>%
        arrange(investor_name, portfolio_name, ald_sector, technology, Line.Type, year) %>% # Tech.Type,
        group_by(investor_name, portfolio_name, ald_sector, technology, Line.Type) %>% # Tech.Type,
        mutate(Green = ifelse(last(Scen2) > last(Scen3), 1, 0))


      if ("Scen4" %in% colnames(ALD.sc.wide)) {
        ALD.sc.wide <- ALD.sc.wide %>% mutate(
          Line2 = ifelse(Green == 1, Scen4, Scen2),
          Line3 = Scen3,
          Line4 = ifelse(Green == 1, Scen2, Scen4)
        )
      }



      if ("Scen1" %in% colnames(ALD.sc.wide) & green_brown(tech_to_plot) == "brown") {
        ALD.sc.wide <- ALD.sc.wide %>% mutate(
          Line1 = ifelse(Green == 1, Scen3, Scen1),
          Line2 = Scen2,
          Line3 = ifelse(Green == 1, Scen1, Scen3)
        )
      } else if ("Scen1" %in% colnames(ALD.sc.wide) & green_brown(tech_to_plot) == "green") {
        ALD.sc.wide <- ALD.sc.wide %>% mutate(
          Line2 = Scen3,
          Line3 = Scen2,
          Line4 = Scen1
        )
      }
    } else if (no.scenarios == 4) {
      ALD.sc.wide <- ALD.sc.wide %>%
        arrange(investor_name, portfolio_name, ald_sector, technology, Line.Type, year) %>% # Tech.Type,
        group_by(investor_name, portfolio_name, ald_sector, technology, Line.Type) %>% # Tech.Type,
        mutate(Green = ifelse(last(Scen2) > last(Scen4), 1, 0))
      if (green_brown(tech_to_plot) == "brown") {
        if (all(ALD.sc.wide[which(ALD.sc.wide$year != 2018), ]$Scen3 < ALD.sc.wide[which(ALD.sc.wide$year != 2018), ]$Scen4)) {
          ALD.sc.wide$Scen3 <- ALD.sc.wide$Scen3
          ALD.sc.wide$Scen4 <- ALD.sc.wide$Scen4
        } else if (all(ALD.sc.wide[which(ALD.sc.wide$year != 2018), ]$Scen3 > ALD.sc.wide[which(ALD.sc.wide$year != 2018), ]$Scen4)) {
          temp <- ALD.sc.wide$Scen3
          ALD.sc.wide$Scen3 <- ALD.sc.wide$Scen4
          ALD.sc.wide$Scen4 <- temp
        }
      } else if (green_brown(tech_to_plot) == "green") {
        if (all(ALD.sc.wide[which(ALD.sc.wide$year != 2018), ]$Scen3 > ALD.sc.wide[which(ALD.sc.wide$year != 2018), ]$Scen4)) {
          ALD.sc.wide$Scen3 <- ALD.sc.wide$Scen3
          ALD.sc.wide$Scen4 <- ALD.sc.wide$Scen4
        } else if (all(ALD.sc.wide[which(ALD.sc.wide$year != 2018), ]$Scen3 < ALD.sc.wide[which(ALD.sc.wide$year != 2018), ]$Scen4)) {
          temp <- ALD.sc.wide$Scen3
          ALD.sc.wide$Scen3 <- ALD.sc.wide$Scen4
          ALD.sc.wide$Scen4 <- temp
        }
      }

      ALD.sc.wide <- ALD.sc.wide %>% mutate(
        Line1 = ifelse(Green == 1, Scen4, Scen1),
        Line2 = ifelse(Green == 1, Scen3, Scen2),
        Line3 = ifelse(Green == 1, Scen2, Scen3),
        Line4 = ifelse(Green == 1, Scen1, Scen4)
      )
    }

    ### IDENTIFY LIMITS of the Y axis
    ymin <- min(ylims$min)
    ymax <- max(ylims$max)

    round.digits <- 1
    if ((ymax - ymin) < 0.01) {
      unit <- 0.001
      round.digits <- 3
    } else if ((ymax - ymin) >= 0.01 & (ymax - ymin) < 0.1) {
      unit <- .01
      round.digits <- 2
    } else if ((ymax - ymin) >= 0.1 & (ymax - ymin) < 1) {
      unit <- .1
    } else if ((ymax - ymin) >= 1 & (ymax - ymin) < 10) {
      unit <- 1
    } else if ((ymax - ymin) >= 10 & (ymax - ymin) < 150) {
      unit <- 10
    } else if ((ymax - ymin) >= 150 & (ymax - ymin) < 1000) {
      unit <- 100
    } else if ((ymax - ymin) >= 1000 & (ymax - ymin) < 10000) {
      unit <- 1000
    } else if ((ymax - ymin) >= 10000 & (ymax - ymin) < 100000) {
      unit <- 10000
    }

    MAX.Y <- ceiling(ymax / unit) * unit
    MIN.Y <- floor(ymin / unit) * unit


    NEXT.LINE <- paste0("Line", as.numeric(max(gsub("Line", "", colnames(ALD.sc.wide)[grep("Line", colnames(ALD.sc.wide))]))) + 1)

    ALD.sc.wide[, NEXT.LINE] <- MAX.Y

    ALD.sc.tall <- ALD.sc.wide %>%
      select(-grep("Scen", colnames(ALD.sc.wide))) %>%
      gather(key = "Target", value = "Value", -investor_name, -portfolio_name, -ald_sector, -technology, -Green, -Line.Type, -year)


    ALD.sc.tall <- ALD.sc.tall %>%
      group_by(investor_name, portfolio_name, ald_sector, technology, Line.Type, Green, year) %>% # Tech.Type
      mutate(
        lower = dplyr::lag(Value),
        lower = ifelse(is.na(lower), MIN.Y, lower)
      )


    ALD.sc.tall <- as.data.frame(ALD.sc.tall)
    ALD.cp <- as.data.frame(ALD.cp)

    # if (no.scenarios == 4){    # WITH 1.75Â°
    green.fill <- c(
      "Line5" = area_1,
      "Line4" = area_2,
      "Line3" = area_2_4,
      "Line2" = area_4_6,
      "Line1" = area_6
    )

    brown.fill <- c(
      "Line5" = area_6,
      "Line4" = area_4_6,
      "Line3" = area_2_4,
      "Line2" = area_2,
      "Line1" = area_1
    )

    green.labels <- c(
      "Line5" = "1D",
      "Line4" = "2D",
      "Line3" = "2D-4D",
      "Line2" = "4D-6D",
      "Line1" = "6D"
    )

    brown.labels <- c(
      "Line5" = "6D",
      "Line4" = "4D-6D",
      "Line3" = "2D-4D",
      "Line2" = "2D",
      "Line1" = "1D"
    )

    outputplot <- ggplot(data = subset(ALD.sc.tall, technology == tech_to_plot & ALD.sc.tall$portfolio_name == PortNames)) +
      geom_ribbon(aes(ymin = lower, ymax = Value, x = year, fill = Target), alpha = 0.75) +
      scale_fill_manual(labels = eval(parse(text = paste(GoodBad, ".labels", sep = ""))), values = eval(parse(text = paste(GoodBad, ".fill", sep = "")))) +
      scale_x_continuous(name = Year, expand = c(0, 0), limits = c(start_year, start_year + 5.6)) +
      scale_y_continuous(
        name = paste0(WeightedProduction, " (", eval(parse(text = paste(GoodBad, ".unit", sep = "")))[tech_to_plot], ")"),
        expand = c(0, 0),
        breaks = round(seq(round(MIN.Y, digits = round.digits), round(MAX.Y, digits = round.digits), length.out = 5), digits = round.digits)
      ) +
      theme_246() +
      theme(legend.position = "none") +
      coord_cartesian(ylim = c(round(MIN.Y, digits = round.digits), round(MAX.Y, digits = round.digits)))


    if (chart_type == "CB") {
      outputplot <- outputplot +
        geom_line(
          data = subset(ALD.cp, technology == tech_to_plot & portfolio_name == portfolio_name_select),
          aes(x = year, y = Production), color = cb_line, size = .75
        ) +
        geom_line(
          data = subset(ALD.cp, technology == tech_to_plot & investor_name == "Market"),
          aes(x = year, y = Production), color = cb_line, size = .75, linetype = "dashed"
        )
    }

    if (chart_type == "EQ") {
      outputplot <- outputplot +
        geom_line(
          data = subset(ALD.cp, technology == tech_to_plot & portfolio_name == portfolio_name_select),
          aes(x = year, y = Production), color = eq_line, size = .75
        ) +
        geom_line(
          data = subset(ALD.cp, technology == tech_to_plot & investor_name == "Market"),
          aes(x = year, y = Production), color = eq_line, size = .75, linetype = "dashed"
        )
    }

    if (chart_type == "BV") {
      outputplot <- outputplot +
        geom_line(
          data = subset(ALD.cp, technology == tech_to_plot & portfolio_name == portfolio_name_select),
          aes(x = year, y = Production), color = bv_line, size = .75
        ) +
        geom_line(
          data = subset(ALD.cp, technology == tech_to_plot & investor_name == "Market"),
          aes(x = year, y = Production), color = bv_line, size = .75, linetype = "dashed"
        )
    }

    if (LegendOn == TRUE) {
      rect_246 <- gList(
        # <1?C
        rectGrob(
          x = 0.09, y = 0.4, width = 0.12, height = 0.3,
          gp = gpar(col = NA, fill = area_1)
        ),
        # 1-2?C
        rectGrob(
          x = 0.21, y = 0.4, width = 0.12, height = 0.3,
          gp = gpar(col = NA, fill = area_2)
        ),
        # 2-4?C
        rectGrob(
          x = 0.33, y = 0.4, width = 0.12, height = 0.3,
          gp = gpar(col = NA, fill = area_2_4)
        ),
        # 4-6?C
        rectGrob(
          x = 0.45, y = 0.4, width = 0.12, height = 0.3,
          gp = gpar(col = NA, fill = area_4_6)
        ),
        # >6?C
        rectGrob(
          x = 0.57, y = 0.4, width = 0.12, height = 0.3,
          gp = gpar(col = NA, fill = area_6)
        )
      )


      arrows_246 <- gList(
        linesGrob(
          x = unit(0.15, "npc"), y = unit(c(0.25, 0.71), "npc"),
          gp = gpar(fill = textcolor),
          arrow = arrow(length = unit(0.15, "cm"), ends = "last", type = "closed")
        ),

        linesGrob(
          x = unit(0.27, "npc"), y = unit(c(0.25, 0.71), "npc"),
          gp = gpar(fill = textcolor),
          arrow = arrow(length = unit(0.15, "cm"), ends = "last", type = "closed")
        ),

        linesGrob(
          x = unit(0.39, "npc"), y = unit(c(0.25, 0.71), "npc"),
          gp = gpar(fill = textcolor),
          arrow = arrow(length = unit(0.15, "cm"), ends = "last", type = "closed")
        ),

        linesGrob(
          x = unit(0.51, "npc"), y = unit(c(0.25, 0.71), "npc"),
          gp = gpar(fill = textcolor),
          arrow = arrow(length = unit(0.15, "cm"), ends = "last", type = "closed")
        )
      )


      if (chart_type == "CB") {
        linecolour <- cb_line
        Line_labels <- c(paste0(BondReference, " portfolio"), paste0(BondReference, " market"))
      } else {
        linecolour <- eq_line
        Line_labels <- c("Equity portfolio", "Listed equity market")
      }


      portfolio_lines <- gList(
        linesGrob(
          x = unit(c(0.65, 0.735), "npc"), y = unit(0.68, "npc"),
          gp = gpar(col = linecolour, lty = 1, lwd = 3)
        ),
        linesGrob(
          x = unit(c(0.65, 0.735), "npc"), y = unit(0.32, "npc"),
          gp = gpar(col = linecolour, lty = 2, lwd = 3)
        )
      )

      ### Legend labels
      # Labels
      scenario_labels <- c("1.75C scenario", "2C scenario", "4C scenario", "6C scenario")
      Legend_labels <- c("<1.75C", "<2C", "1.75 - 2C", "2 - 4C", "4 - 6C", "6C")

      # Positions
      scenario_labels_position <- c(0.15, 0.27, 0.39, 0.51)
      Legend_labels_position <- c(0.09, 0.21, 0.33, 0.45, 0.57)
      Line_labels_position <- c(0.68, 0.32)

      # Label scenario and 2-4-6
      label_text_246 <- gList(
        textGrob(scenario_labels,
          x = scenario_labels_position, y = 0.88,
          just = "center", gp = gpar(fontsize = 5.8, col = textcolor)
        ),

        textGrob(Legend_labels[c(1, 3:6)],
          x = Legend_labels_position, y = 0.4,
          just = "center", gp = gpar(fontsize = 7, col = textcolor)
        ),

        textGrob(scenario_labels[2:4],
          x = scenario_labels_position[2:4], y = 0.8,
          just = "center", gp = gpar(fontsize = 5.8, col = textcolor)
        ),

        textGrob(Legend_labels[c(2, 4:6)],
          x = Legend_labels_position[2:5], y = 0.4,
          just = "center", gp = gpar(fontsize = 7, col = textcolor)
        )
      )



      # Label line
      label_text_lines <- gList(
        textGrob(Line_labels,
          x = 0.745, y = Line_labels_position,
          just = "left", gp = gpar(fontsize = 7, col = textcolor)
        )
      )


      if (no.scenarios == 3) {
        footer_246 <- grobTree(
          rectGrob(gp = gpar(fill = "white", lwd = 0, col = "white")),
          rect_246[2:5], arrows_246[2:4], label_text_246[3:4], portfolio_lines[1:2], label_text_lines[1]
        )

        # outputplot <- arrangeGrob(outputplot, footer_246, heights = unit(c(0.85, 0.15), c('npc', 'npc'))) #
        outputplot <- arrangeGrob(outputplot, footer_246, heights = unit(c(0.85, 0.15), c("npc", "npc"))) #
      } else if (no.scenarios == 4) {
        footer_1246 <- grobTree(
          rectGrob(gp = gpar(fill = "white", lwd = 0, col = "white")),
          rect_246, arrows_246, label_text_246[1:2], portfolio_lines[1:2], label_text_lines[1]
        )
        # outputplot <- arrangeGrob(outputplot, footer_1246, heights = unit(c(0.85, 0.15), c('npc', 'npc'))) #
        outputplot <- arrangeGrob(outputplot, footer_1246, heights = unit(c(0.85, 0.15), c("npc", "npc"))) #
      }
    }
  } else {
    Label <- YourportfoliohasnoproductionfortheselectedTechnologyScenarioGeographyandMarket

    outputplot <- no_chart(Label)
  }


  ggsave(outputplot, filename = graph_name(plotnumber, ParameterFile, explicit_filename = explicit_filename), bg = "white", height = 3.6, width = 4.6, dpi = ppi * 2) # ,bg="white"

  tech_to_plot <<- ""
  BV.asset_type <<- ""
}

CO2IntensityTrend <- function(plotnumber, sector_to_plot, LegendOn = TRUE, explicit_filename = "") {
  PlotChart <- FALSE
  Data <- NA
  Data_debt <- NA
  sector_to_plot <<- sector_to_plot

  if (GraphType == "Report") {
    LegendOn <- FALSE
  } else {
    LegendOn <- TRUE
  }

  if (has_equity == TRUE) {
    Data <- filter_by_parameters(EQCombin, "EQ", byscenario = F)
    if (data_check(Data) == TRUE) {
      Data$Type <- "equity"
    } else {
      print("no EQ")
    }
  }

  if (has_debt == TRUE) {
    Data_debt <- filter_by_parameters(CBCombin, "CB", by_equity_market = FALSE, byscenario = F)
    if (data_check(Data_debt) == TRUE) {
      Data_debt$Type <- BondReference
    } else {
      print("no CB")
    }
  }

  if (data_check(Data) == TRUE & data_check(Data_debt) == TRUE) {
    Data <- subset(Data, select = colnames(Data_debt))
    Data <- rbind(Data, Data_debt)
    Data <- subset(Data, year >= start_year & year <= start_year + 5)
  } else if (data_check(Data) == FALSE & data_check(Data_debt) == TRUE) {
    Data <- Data_debt
  } else if (data_check(Data) == TRUE & data_check(Data_debt) == FALSE) {
    Data <- Data
  }


  if (data_check(Data) == TRUE) {
    Data <- Data[!is.na(Data$plan_sec_emissions_factor), ]
  }

  if (data_check(Data) == TRUE) {
    Data <- Data[Data$ald_sector == sector_to_plot, ]
    if (data_check(Data) == TRUE) {
      PlotChart <- TRUE
    } else {
      PlotChart <- FALSE
    }
  }


  if (PlotChart == TRUE) {
    # print(PlotChart)



    if (sector_to_plot == "Aviation") {
      PlotData <- Data %>%
        ungroup() %>%
        select(portfolio_name, year, technology, plan_sec_emissions_factor, scen_sec_emissions_factor, Type) %>%
        group_by(portfolio_name, year, Type) %>%
        filter(year <= start_year + 5) %>%
        summarise(
          plan_emission_factor = mean(plan_sec_emissions_factor, na.rm = T) * 1000,
          scen_emission_factor = mean(scen_sec_emissions_factor, na.rm = T) * 1000
        )

      Ref.Data <- PlotData %>%
        ungroup() %>%
        filter(year == start_year) %>%
        rename(Ref.Scen.EF = scen_emission_factor) %>%
        select(Type, Ref.Scen.EF)

      PlotData <- PlotData %>% left_join(Ref.Data, by = "Type")

      PlotData <- PlotData %>%
        group_by(year, Type) %>%
        mutate(scen_emission_factor = scen_emission_factor * plan_emission_factor / Ref.Scen.EF) %>%
        select(-Ref.Scen.EF)
    } else {
      PlotData <- Data %>%
        ungroup() %>%
        select(portfolio_name, year, technology, plan_sec_emissions_factor, scen_sec_emissions_factor, Type) %>%
        group_by(portfolio_name, year, Type) %>%
        filter(year <= start_year + 5) %>%
        summarise(
          plan_emission_factor = mean(plan_sec_emissions_factor, na.rm = T),
          scen_emission_factor = mean(scen_sec_emissions_factor, na.rm = T)
        )

      Ref.Data <- PlotData %>%
        ungroup() %>%
        filter(year == start_year) %>%
        rename(Ref.Scen.EF = scen_emission_factor) %>%
        select(Type, Ref.Scen.EF)

      PlotData <- PlotData %>% left_join(Ref.Data, by = "Type")

      PlotData <- PlotData %>%
        group_by(year, Type) %>%
        mutate(scen_emission_factor = scen_emission_factor * plan_emission_factor / Ref.Scen.EF) %>%
        select(-Ref.Scen.EF)
    }

    # print(nrow(PlotData))
    OtherSectorUnits <- data.frame("ald_sector" = c("Cement", "Steel", "Aviation"), "Units" = c(
      paste0("t CO2 / t ", L_Cement), #
      paste0("t CO2 / t ", L_Steel), #
      paste0("CO2/", x_passenger, "km")
    )) #

    year_lab <- Year #   "year" #GT["year"][[1]]
    ylabel <- paste0(EmissionsIntensity, OtherSectorUnits$Units[OtherSectorUnits$ald_sector == sector_to_plot], ")") #

    df <- PlotData

    colourdf <- data.frame("Colour" = c(eq_line, cb_line), "TargetCol" = c(area_2, area_2), "Type" = c("equity", BondReference))
    df <- base::merge(df, colourdf, by = "Type")

    outputplot <- ggplot() +
      geom_line(data = df, aes(x = year, y = scen_emission_factor, colour = Type, group = Type), size = 1.5, linetype = 1, colour = area_2) +
      annotate(geom = "point", y = df$plan_emission_factor[df$year == start_year], x = df$year[df$year == start_year], size = 5, colour = unique(df$Colour), fill = unique(df$Colour), shape = 22)

    outputplot <- outputplot +
      # scale_fill_identity(name = "", guide = 'legend',labels = c("Exposure gap","Current capacity + planned additions")) +
      scale_colour_manual(name = "", guide = "legend", values = unique(df$Colour), labels = unique(df$Type)) +
      xlab(year_lab) + ylab(ylabel) + # Set axis labels
      # legend(values=legelabels)+
      scale_x_continuous(breaks = seq(start_year, max(df$year), 1), expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      expand_limits(x = c(start_year - 0.5, start_year + 5.5), y = c(.7 * min(df[, c("plan_emission_factor", "scen_emission_factor")], na.rm = TRUE), 1.3 * max(df[, c("scen_emission_factor")], na.rm = TRUE))) +
      theme_linecharts() #+
    # theme(axis.line.x = element_line(colour = "red",size=0.5))

    if (LegendOn == TRUE) {

      # Count number of sectors present
      nosectors <- length(unique(df$Type))
      if (nosectors == 1) {
        chartsectors <- unique(df$Type)
        if (chartsectors == "equity") {
          boxcol <- eq_line
        } else if (chartsectors == "Corporate bond") {
          boxcol <- cb_line
        }
      }


      if (nosectors == 2) {
        y1 <- 0.8
        y2 <- 0.5
        y3 <- 0.2
      } else {
        y3 <- 0.4
        y2 <- 0.8
      }
      boxdim <- c(0.03, 0.2)


      ### Target emissions intensity lines
      Emission_lines <- gList(
        linesGrob(
          x = unit(c(0.03, 0.13), "npc"), y = unit(y3, "npc"),
          gp = gpar(col = area_2, lty = 1, lwd = 3)
        )
      )

      ### Current Portfolio Emission Intensity

      if (nosectors == 2) {
        Emission_rectangle <- gList(
          rectGrob(
            x = 0.07, y = unit(y2, "npc"), width = boxdim[1], height = boxdim[2],
            gp = gpar(col = NA, fill = eq_line)
          ),
          rectGrob(
            x = 0.07, y = unit(y1, "npc"), width = boxdim[1], height = boxdim[2],
            gp = gpar(col = NA, fill = cb_line)
          )
        )
      } else {
        Emission_rectangle <- gList(
          rectGrob(
            x = 0.07, y = unit(y2, "npc"), width = boxdim[1], height = boxdim[2],
            gp = gpar(col = NA, fill = boxcol)
          )
        )
      }

      ### legend labels
      # labels
      if (nosectors == 2) {
        Emission_labels <- c(
          paste0("Current emissions intensity for your", tolower(BondReference), "portfolio"),
          "Current emissions intensity for your equity portfolio",
          "Target emissions intensity for the relevant portfolio"
        )
        Emission_labels_position <- c(y1, y2, y3)
      } else {
        Emission_labels <- c(
          paste0("Current emissions intensity for your ", chartsectors, " portfolio"),
          "Target emissions intensity for the portfolio"
        )
        Emission_labels_position <- c(y2, y3)
      }
      # positions

      # all labels together
      Emission_label_text <-
        textGrob(Emission_labels,
          x = 0.15, y = Emission_labels_position,
          just = "left", gp = gpar(fontsize = 8, col = textcolor, family = textfont)
        )

      # all legend objects togeter
      # footer_Emission = grobTree(rectGrob(gp = gpar(fill = 'transparent', lwd = 0, colour = "white")),
      #   Emission_lines, Emission_rectangle, Emission_label_text) # one footer too much

      footer_Emission <- grobTree(Emission_lines, Emission_rectangle, Emission_label_text)

      # outputplot <- arrangeGrob(footer_Emission, heights = unit(c(.15), c('npc')))
      # legend with "a" - a ggplot object
      outputplot <- arrangeGrob(outputplot, footer_Emission, heights = unit(c(0.85, 0.15), c("npc", "npc")))
    }
  } else {
    if (sector_to_plot == "Cement") {
      Label <- paste0(TheportfoliohasnocompaniesintheCementsector) #
    } else if (sector_to_plot == "Steel") {
      Label <- paste0(TheportfoliohasnocompaniesintheSteelsector) #
    } else if (sector_to_plot == "Aviation") {
      Label <- paste0(TheportfoliohasnocompaniesintheAviationsector) #
    } else { # (sector_to_plot == "Shipping")
      Label <- paste0(TheportfoliohasnocompaniesintheShippingsector) #
    }

    outputplot <- no_chart(Label)
  }

  ggsave(filename = graph_name(plotnumber, ParameterFile, explicit_filename = explicit_filename), bg = "white", outputplot, height = 3, width = 4, dpi = ppi) # ,bg="transparent"
  sector_to_plot <<- ""

  # return()
}

CompanyInformation <- function(plotnumber, companiestoprint, chart_type, sector_to_plot, explicit_filename = "") {
  sector_to_plot <<- sector_to_plot
  PlotChart <- F

  if (chart_type == "EQ" & has_equity == T) {
    market <- filter_by_parameters(eq_market, "EQ")
    combin <- filter_by_parameters(EQCombin, "EQ")
    CompProdSS <- filter_by_parameters(EQCompProdSnapshot, "EQ", ActorSectorOnly = T)
  }
  if (chart_type == "CB" & has_debt == T) {
    market <- filter_by_parameters(cb_market, "CB", by_equity_market = FALSE)
    combin <- filter_by_parameters(CBCombin, "CB", by_equity_market = FALSE)
    CompProdSS <- filter_by_parameters(CBCompProdSnapshot, "CB", by_equity_market = FALSE, ActorSectorOnly = T)
  }

  if (data_check(CompProdSS) == T) {
    PlotChart <- T
  }


  if (PlotChart == TRUE) {
    if (!"Entity.Name" %in% colnames(CompProdSS) & chart_type == "CB") {
      CompProdSS$Entity.Name <- CompProdSS$id
    }
    if (!"Entity.Name" %in% colnames(CompProdSS) & chart_type == "EQ") {
      CompProdSS$Entity.Name <- CompProdSS$company_name
    }


    # Portfolio (Weighted by the AUM)
    Portfoliomix <- subset(combin, year == start_year + 5, select = c("technology", "plan_alloc_wt_tech_prod"))
    Portfoliomix <- unique(Portfoliomix) # KLAUS: Added this line to avoid duplicate lines!
    Portfoliomix$Classification <- "Portfolio"
    Portfoliomix$Name <- "Portfolio"
    Portfoliomix <- subset(Portfoliomix, select = c("Name", "Classification", "technology", "plan_alloc_wt_tech_prod"))
    Portfoliomix$plan_alloc_wt_tech_prod <- Portfoliomix$plan_alloc_wt_tech_prod
    colnames(Portfoliomix) <- c("Name", "Classification", "technology", "tech_share")

    # Add 2D Target (Global Market under 2D scenario)
    Targetmix <- subset(combin, year == start_year + 5, select = c("technology", "scen_alloc_wt_tech_prod")) # Scen.WtProduction needs to be adjusted to BenchmarkPortfolio (Trajectory vs Market approach) - will address this in FilterByParameters
    Targetmix <- unique(Targetmix) # KLAUS: Added this line to avoid duplicate lines!
    Targetmix$Classification <- "Portfolio"
    Targetmix$Name <- "Aligned Portfolio"
    Targetmix <- subset(Targetmix, select = c("Name", "Classification", "technology", "scen_alloc_wt_tech_prod"))
    Targetmix$scen_alloc_wt_tech_prod <- Targetmix$scen_alloc_wt_tech_prod
    colnames(Targetmix) <- c("Name", "Classification", "technology", "tech_share")

    # Add Benchmark / Global Market

    Marketmix <- NA
    if (data_check(market)) {
      Marketmix <- subset(market, year == start_year + 5, select = c("technology", "scen_alloc_wt_tech_prod"))
      Marketmix <- unique(Marketmix)
      Marketmix$Classification <- "Portfolio"
      Marketmix$Name <- "Aligned Market"
      Marketmix <- subset(Marketmix, select = c("Name", "Classification", "technology", "scen_alloc_wt_tech_prod"))
      Marketmix$scen_alloc_wt_tech_prod <- Marketmix$scen_alloc_wt_tech_prod
      colnames(Marketmix) <- c("Name", "Classification", "technology", "tech_share")
    }

    PortfolioData <- rbind(Marketmix, Targetmix, Portfoliomix)
    # PortfolioData <- rbind(Targetmix, Portfoliomix)

    # Percentage share of each technology for each company in the portfolio
    Companies <- subset(CompProdSS, year == start_year + 5, select = c("Entity.Name", "technology", "plan_tech_prod", "plan_sec_prod", "port_weight"))
    Companies <- Companies %>%
      filter(plan_sec_prod > 0) %>%
      select(-plan_sec_prod)
    Companies <- unique(Companies)
    Companies$tech_share <- Companies$plan_tech_prod
    Companies$Classification <- "Companies"
    # Companies$Name <- paste0(substr(Companies$Name, 1, 15),"...")
    Companies <- subset(Companies, select = c("Entity.Name", "Classification", "technology", "tech_share", "port_weight"))
    colnames(Companies) <- c("Name", "Classification", "technology", "tech_share", "PortWeight")
    Companies$Name <- as.character(Companies$Name)
    Companies$Name <- ifelse(is.na(Companies$Name), "No Name", Companies$Name)

    colors <- as.vector(ColourPalette$Colours[ColourPalette$ald_sector %in% sector_to_plot])

    if (sector_to_plot == "Power") {
      techorder <- c("RenewablesCap", "HydroCap", "NuclearCap", "GasCap", "CoalCap")
      tech_labels <- c(T_Renewables, T_HydroCap, T_Nuclear, T_GasCap, T_CoalCap) #
      # colors <- as.vector(ColourPalette$Colours[ColourPalette$ald_sector %in% sector_to_plot])
    } else if (sector_to_plot == "Automotive") {
      techorder <- c("Electric", "Hybrid", "ICE")
      tech_labels <- c(x_Electric, x_Hybrid, x_ICE) #
    } else if (sector_to_plot == "CoalMining") {
      techorder <- c("Coal")
      tech_labels <- c(T_CoalProd) #
      colors <- as.vector(ColourPalette$Colours[ColourPalette$technology %in% "Coal"])
    } else if (sector_to_plot == "OilGas") {
      techorder <- c("Gas", "Oil")
      tech_labels <- c(T_GasProd, T_OilProd) #
      colors <- as.vector(ColourPalette$Colours[ColourPalette$technology %in% tech_labels])
    } else if (sector_to_plot == "Shipping") {
      tech_labels <- c("GHG_A", "GHG_B", "GHG_C", "GHG_D", "GHG_E", "GHG_F", "GHG_G")
      # colors <- as.vector(ColourPalette$Colours[12:13])
    } else if (sector_to_plot == "Steel") {
      tech_labels <- c(ElectricArc, BlastFurnace) #
      # colors <- as.vector(ColourPalette$Colours[14:20])
    }


    PortfolioData <- filter(PortfolioData, technology %in% techorder)
    Companies <- Companies %>%
      filter(technology %in% techorder) %>%
      arrange(-PortWeight)

    Companies <- Companies %>%
      filter(Name %in% unique(Companies$Name)[1:min(companiestoprint, length(unique(Companies$Name)))])

    dummy <- data.frame(
      c("Name", ""),
      c("Classification", NA),
      c("technology", NA),
      c("tech_share", 0)
    )
    colnames(dummy) <- as.character(unlist(dummy[1, ]))
    dummy <- dummy[-1, ]
    dummy$tech_share <- as.numeric(dummy$tech_share)

    AllData <- rbind(
      PortfolioData,
      dummy,
      select(Companies, -PortWeight)
    )
    AllData <- unique(AllData)
    AllData$Name <- factor(AllData$Name, levels = rev(unique(c(PortfolioData$Name, "", Companies$Name))))
    AllData$Name <- dplyr::recode(AllData$Name,
      "Aligned Market"  = AlignedMarket, #
      "Aligned Portfolio" = AlignedPortfolio, #
      Portfolio = Portf
    ) #
    AllData$technology <- factor(AllData$technology, levels = rev(techorder))

    names(colors) <- techorder
    names(tech_labels) <- techorder
    Companies <- unique(select(Companies, Name, PortWeight, technology))
    PortfolioData <- unique(select(PortfolioData, Name))
    PortfolioData$Name <- dplyr::recode(PortfolioData$Name,
      "Aligned Market" = AlignedMarket,
      "Aligned Portfolio" = AlignedPortfolio,
      Portfolio = Portf
    )
    bar_labels <- c(PortfolioData$Name, "", Companies$Name)

    # company_labels <- trim(Companies$Name)
    company_labels <- Companies$Name
    for (i in 1:length(company_labels)) {
      if (str_length(company_labels[i]) > 15) {
        new_name <- strtrim(company_labels[i], 15)
        company_labels[i] <- paste0(new_name, "...")
      }
    }


    bar_labels <- c(PortfolioData$Name, "", company_labels)

    Companies_PortWeight <- Companies %>%
      group_by(Name) %>%
      summarise(PortWeight = mean(PortWeight))


    PortPlot <- stacked_bar_chart(AllData, colors, tech_labels) +
      geom_text(
        data = Companies_PortWeight,
        aes(x = Name, y = 1),
        label = perc(Companies_PortWeight$PortWeight),
        hjust = -1., color = textcolor, size = 10 * (5 / 14),
        family = textfont
      ) +
      geom_text(
        data = Companies,
        aes(x = "", y = 1),
        label = x_Weight, #
        hjust = -0.9, color = textcolor, size = 10 * (5 / 14),
        family = textfont
      ) +
      xlab("") +
      coord_flip() +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(l = 0.15, r = 0.35, unit = "cm")),
        plot.margin = unit(c(1, 8, 0, 0), "lines")
      )

    gt <- ggplot_gtable(ggplot_build(PortPlot))
    gt$layout$clip[gt$layout$name == "panel"] <- "off"

    if (sector_to_plot == "Fossil Fuels") {
      sector_to_plot <- "FossilFuels"
    }

    bar_size <- 8 / 15
    # height <- min(4+companiestoprint,n_distinct(AllData$Name))*bar_size + 2

    if (sector_to_plot == "Fossil Fuels") {
      sector_to_plot <- "FossilFuels"
    }

    height <- max(2.6, length(unique(Companies$Name)) * .3)
  } else {
    if (sector_to_plot == "Automotive") {
      Label <- paste0(YourPortfoliohasnocompaniesintheAutomotivesector) #
    } else if (sector_to_plot == "CoalMining") {
      Label <- paste0(YourPortfoliohasnocompaniesintheCoalMiningsector) #
    } else if (sector_to_plot == "OliGas") {
      Label <- paste0(YourPortfoliohasnocompaniesintheOilGassector) #
    } else if (sector_to_plot == "Power") {
      Label <- paste0(YourPortfoliohasnocompaniesinthePowersector) #
    } else if (sector_to_plot == "Shipping") {
      Label <- paste0(YourPortfoliohasnocompaniesintheShippingsector) #
    } else if (sector_to_plot == "Steel") {
      Label <- paste0(YourPortfoliohasnocompaniesintheSteelsector) #
    }

    gt <- no_chart(Label)
    height <- 3
  }
  ggsave(gt,
    filename = graph_name(plotnumber, ParameterFile, explicit_filename = explicit_filename), # bg = "transparent",
    height = height, width = 10, dpi = ppi
  )

  sector_to_plot <<- ""
  BV.asset_type <<- ""
}

OilShare <- function(plotnumber, companiestoprint, chart_type, explicit_filename = "") {
  tech_to_plot <<- "Oil"
  chart_type <<- chart_type

  OilData <- NA

  if (chart_type == "EQ") {
    comps <- filter_by_parameters(EQCompProdSnapshot, "EQ") %>% ungroup()
    # comps$id<-comps$bloomberg_id
    # comps$bloomberg_id <-NULL
    # comps$id <-as.integer(comps$id)

    if (data_check(comps)) {
      OilData <- EQOilShareData
      OilData <- OilData[OilData$id %in% comps$id, ]
      OilData <- OilData[OilData$technology == "Oil", ]
      OilData <- OilData[OilData$year == start_year + 5, ]

      OilData <- OilData %>%
        group_by(id, year) %>%
        mutate(SecOilProd = sum(ald_production)) %>%
        filter(SecOilProd > 0) %>%
        select(-SecOilProd) %>%
        ungroup()


      if (data_check(OilData)) {
        OilData$Type <- "equity"
        OilData$ID <- OilData$id
        OilData$bloomberg_id <- NULL
      }

      OilData <- left_join(comps %>% select(technology, company_name, id, port_weight), OilData, by = c("technology" = "technology", "id" = "id"))
    }
  }

  if (chart_type == "CB") {
    comps <- filter_by_parameters(CBCompProdSnapshot, "CB", by_equity_market = F)
    # comps$id <- comps$corp_bond_ticker

    if (data_check(comps)) {
      OilData <- CBOilShareData %>%
        ungroup() %>%
        filter(
          technology == "Oil",
          year == start_year + 5,
          id %in% comps$id
        )

      OilData <- OilData %>%
        group_by(id, year) %>%
        mutate(SecOilProd = sum(ald_production)) %>%
        filter(SecOilProd > 0) %>%
        select(-SecOilProd) %>%
        ungroup()

      if (data_check(OilData)) {
        OilData <- OilData %>% mutate(
          Type = "corporate bond",
          ID = id,
          company_name = ID,
          company_lvl_production = ald_production
        )

        OilData <- left_join(comps %>% select(technology, id, port_weight), OilData, by = c("technology" = "technology", "id" = "id"))
      }
    }
  }


  if (data_check(OilData)) {
    PlotChart <- TRUE
  } else {
    PlotChart <- FALSE
  }

  if (PlotChart) {
    OilCompanies <- subset(OilData, select = c("ald_production", "port_weight", "technology_type", "company_name"))

    OilCompanies1 <- OilCompanies %>%
      group_by(company_name, technology_type, port_weight) %>%
      summarise("Oilsum" = sum(ald_production)) %>%
      ungroup()
    OilCompanies2 <- OilCompanies1 %>%
      group_by(company_name) %>%
      summarise("Oiltotal" = sum(Oilsum))

    OilCompanies <- left_join(OilCompanies1, OilCompanies2, by = c("company_name"))

    OilCompanies$OilShare <- (OilCompanies$Oilsum / OilCompanies$Oiltotal)
    OilCompanies$Classification <- "Companies"


    OilCompanies$technology_type <- ifelse(OilCompanies$technology_type == "Conventional Gas", "Other & Unknown", OilCompanies$technology_type)
    OilCompanies$technology_type <- ifelse(OilCompanies$technology_type == "Unconventional Gas", "Other & Unknown", OilCompanies$technology_type)
    OilCompanies$technology_type <- ifelse(OilCompanies$technology_type == "-", "Other & Unknown", OilCompanies$technology_type)

    OilCompanies$technology_type <- ifelse(OilCompanies$technology_type == "Oil Sands", OilSands, OilCompanies$technology_type)
    OilCompanies$technology_type <- ifelse(OilCompanies$technology_type == "Heavy Oil", HeavyOil, OilCompanies$technology_type)
    OilCompanies$technology_type <- ifelse(OilCompanies$technology_type == "Conventional Oil", ConventionalOil, OilCompanies$technology_type)
    OilCompanies$technology_type <- ifelse(OilCompanies$technology_type == "Unconventional Oil", UnconventionalOil, OilCompanies$technology_type)
    OilCompanies$technology_type <- ifelse(OilCompanies$technology_type == "Other & Unknown", OtherandUnknown, OilCompanies$technology_type)



    OilCompanies$technology_type <- as.factor(OilCompanies$technology_type)
    # levels(OilCompanies$technology_type)[levels(OilCompanies$technology_type)=="Conventional Gas"] <- "Other & Unknown"
    # levels(OilCompanies$technology_type)[levels(OilCompanies$technology_type)=="Unconventional Gas"] <- "Other & Unknown"
    # levels(OilCompanies$technology_type)[levels(OilCompanies$technology_type)=="-"] <- "Other & Unknown"

    # techorder <- c("Oil Sands","Heavy Oil","Conventional Oil","Unconventional Oil","Other & Unknown")
    techorder <- c(OilSands, HeavyOil, ConventionalOil, UnconventionalOil, OtherandUnknown)

    tech_labels <- techorder

    colors <- c("#72755e", "#8d9176", "#a5a792", "#bcbeae", "#d3d5ca")

    OilCompanies$technology_type <- factor(OilCompanies$technology_type, levels = techorder)

    OilCompanies <- OilCompanies %>%
      filter(technology_type %in% techorder) %>%
      arrange(-port_weight)

    # LIne to aggregate to portfolio level
    # aggregated all companies by oil type to get the portfolio oilshare
    # OilCompanies_P<-OilCompanies %>%
    #   group_by(technology_type) %>%
    #   summarise(OilShare=sum(OilShare)) %>%
    #   ungroup()%>%
    #   mutate(company_name="Portfolio",
    #          Classification="Portfolio")
    # colnames(OilCompanies_P)[which(names(OilCompanies_P) == "technology_type")] <- "Oil.Type"

    OilCompanies <- OilCompanies %>%
      filter(company_name %in% unique(OilCompanies$company_name)[1:min(companiestoprint, length(unique(OilCompanies$company_name)))])

    colnames(OilCompanies)[which(names(OilCompanies) == "technology_type")] <- "Oil.Type"
    OilCompanies <- subset(OilCompanies, select = c("Oil.Type", "company_name", "OilShare", "Classification", "port_weight"))

    dummy <- data.frame(
      c("Oil.Type", NA),
      c("company_name", ""),
      c("OilShare", 0),
      c("Classification", NA),
      c("port_weight", NA)
    )

    colnames(dummy) <- as.character(unlist(dummy[1, ]))
    dummy <- dummy[-1, ]
    dummy$OilShare <- as.numeric(dummy$OilShare)

    OilCompany <- rbind(OilCompanies, dummy)
    OilCompany$Oil.Type <- factor(OilCompany$Oil.Type, levels = techorder)
    OilCompany$port_weight <- as.numeric(OilCompany$port_weight)
    OilCompany <- as.data.frame(OilCompany)
    # OilCompany$company_name <- factor(OilCompany$company_name, levels=(unique(c("",OilCompany$company_name))))


    names(colors) <- techorder
    names(tech_labels) <- techorder

    # company_labels <- c(trimws(unique(OilCompanies[!is.na(OilCompanies$Oil.Type),]$company_name))," ","Portfolio")
    Oil <- na.omit(OilCompany[, c("company_name", "port_weight")])
    OilCompany <- OilCompany %>% arrange(port_weight)
    company_labels <- trimws(unique(OilCompany[!is.na(OilCompany$Oil.Type), ]$company_name))

    for (i in 1:length(company_labels)) {
      if (str_length(company_labels[i]) > 15) {
        new_name <- strtrim(company_labels[i], 15)
        company_labels[i] <- paste0(new_name, "...")
      }
    }

    bar_labels <- c(company_labels, "                          ") # company_labels#

    OilCompany <- OilCompany %>% filter(!is.na(port_weight))

    PortPlot <- ggplot(data = OilCompany, aes(x = company_name, y = OilShare, fill = Oil.Type)) +
      geom_bar(position = "fill", stat = "identity") +
      scale_fill_manual(values = colors, labels = rev(paste(tech_labels, " ")), breaks = rev(techorder)) +
      scale_y_continuous(expand = c(0, 0), labels = percent) +
      scale_x_discrete(labels = bar_labels) +
      guides(fill = guide_legend(nrow = 1)) +
      theme_barcharts() +
      geom_text(
        data = OilCompany,
        aes(x = company_name, y = 1),
        label = perc(OilCompany$port_weight),
        hjust = -1, color = textcolor, size = 8 * (5 / 14),
        family = textfont
      ) +
      # geom_text(aes(x="",y=1),
      #           label = x_Weight,
      #           hjust = -0.5, color = textcolor, size=8*(5/14),
      #           family = textfont)+
      xlab("") +
      ylab("tech_share") +
      coord_flip() +
      theme(
        legend.position = "bottom", legend.title = element_blank(),
        plot.margin = unit(c(1, 6, 0, 0), "lines"), axis.line.x = element_line(colour = textcolor, size = 0.5)
      ) +
      guides(fill = guide_legend(ncol = 5, keywidth = 1))



    gt <- ggplot_gtable(ggplot_build(PortPlot))
    gt$layout$clip[gt$layout$name == "panel"] <- "off"

    bar_size <- 4 / 15
    height <- min(1 + companiestoprint, n_distinct(OilCompanies$company_name)) * bar_size + .86
  } else {
    if (chart_type == "EQ") {
      Type <- oil_equity
    } else if (chart_type == "CB") {
      Type <- oil_bond
    } #

    Label <- paste0(Nooilproductionbreakdowninyour, " ", Type, portfo) #
    gt <- no_chart(Label) #+
    height <- 3
  }

  ggsave(gt, filename = graph_name(plotnumber, ParameterFile, explicit_filename = explicit_filename), bg = "white", height = height, width = 10, dpi = ppi)

  tech_to_plot <<- ""
  chart_type <<- ""
  BV.asset_type <<- ""
}

MapChart <- function(plotnumber, chart_type, tech_to_plot, plot_year, explicit_filename = "") {
  Power <- data.frame()
  tech_to_plot <<- tech_to_plot
  # ScenarioGeographyChoose <<- "Global"
  mapfilename <- ""

  if (chart_type == "EQ" & has_equity == TRUE) {
    Power <- filter_by_parameters(EQportmap, "EQ", byscenario = F)
    mapfilename <- paste0(results_path, "/", investor_name_select, "/", "Equiry_results_map.rda")
  }
  if (chart_type == "CB" & has_debt == TRUE) {
    Power <- filter_by_parameters(CBportmap, "CB", byscenario = F, by_equity_market = FALSE)
    mapfilename <- paste0(results_path, "/", investor_name_select, "/", "Bonds_results_map.rda")
  }

  if (data_check(Power)) {
    PlotChart <- TRUE
  } else {
    PlotChart <- FALSE
  }


  if (!file.exists(mapfilename)) {
    PlotChart <- FALSE
  }



  if (PlotChart == TRUE) {
    # tech_to_plot <- gsub("Cap","",tech_to_plot)
    Power <- as.data.frame(Power)
    # Power$technology<-gsub("RenewablesCap","Renewables",Power$technology)
    # Power$technology<-gsub("NuclearCap","Nuclear",Power$technology)
    # Power$technology<-gsub("HydroCap","Hydro",Power$technology)

    Power$technology <- as.factor(Power$technology)
    Power <- subset(Power, year == plot_year)


    Power <- Power %>%
      group_by(ald_location, technology) %>%
      summarise(Production = sum(plan_alloc_wt_tech_prod)) %>%
      ungroup() %>%
      filter(technology == tech_to_plot) %>%
      arrange(desc(Production))
    Power <- as.data.frame(Power)
    UNIT <- c("MW", "MW", "MW", "MW", "MW", Vehicles, Vehicles, Vehicles, "m3", "bbl", MetricTons) #
    names(UNIT) <- c("RenewablesCap", "HydroCap", "NuclearCap", "GasCap", "CoalCap", "Electric", "Hybrid", "ICE", "Gas", "Oil", "Coal")


    if (sector_to_plot %in% c("Power")) {
      capprod <- Power_Unit
    } else {
      capprod <- xProduction
    } #


    # if (nrow(Power)>0){
    tech_map <- joinCountryData2Map(Power, joinCode = "ISO2", nameJoinColumn = "ald_location")
    tech_map_poly <- fortify(tech_map) # extract polygons
    tech_map_poly <- base::merge(tech_map_poly, tech_map@data, by.x = "id", by.y = "ADMIN", all.x = T)
    tech_map_poly <- tech_map_poly %>% arrange(id, order)

    outputplot <- ggplot() +
      coord_map(xlim = c(-180, 180), ylim = c(-60, 75)) +
      geom_polygon(data = tech_map_poly, aes(long, lat,
        group = group,
        fill = Production
      ), size = 0.3) +
      scale_fill_gradient(
        name = paste0(capprod, " (", UNIT[tech_to_plot], ")"),
        # breaks=floor(seq(0,max(Power$Production),length.out = 3)),
        low = eval(parse(text = paste("MIN", tech_to_plot, sep = ""))),
        high = eval(parse(text = paste("MAX", tech_to_plot, sep = ""))),
        labels = comma
      ) +
      theme_bw() +
      xlab(NULL) +
      ylab(NULL) +
      guides(fill = guide_colorbar(
        keywidth = 0.7, keyheight = 0.7,
        reverse = F, title.position = "top"
      )) +
      theme(
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "right",
        legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.8, "cm")
      )
    #
    #     mapWorld <- borders("world", colour="#E7E7E7", fill="#1a0000", alpha = .5)
    #     outputplot<- ggplot()+mapWorld+
    #       geom_point(aes(x=Power$Longitude, y=Power$Latitude, size=Power$Production), alpha = .9, color="#000033")+
    #       labs(size = "Production")+
    #       scale_size_continuous(breaks = round(unname(quantile(Power$Production)),digits = 0))+
    #       theme(axis.title.x=element_blank(),
    #             axis.text.x=element_blank(),
    #             axis.ticks.x=element_blank(),
    #             axis.title.y=element_blank(),
    #             axis.text.y=element_blank(),
    #             axis.ticks.y=element_blank())
  } else {
    Label <- YourportfoliohasnoproductionfortheselectedTechnologyScenarioGeographyandMarket #
    outputplot <- no_chart(Label)
  }

  ggsave(plot = outputplot, filename = graph_name(plotnumber, ParameterFile, explicit_filename = explicit_filename), bg = "white", height = 5, width = 10, dpi = ppi) # linewidth_in*.9

  tech_to_plot <<- ""
  BV.asset_type <<- ""
}

PeerComparison <- function(plotnumber, VariableToPlot = "plan_carsten", Grouping.Level = "portfolio_name", chart_type, BV.asset_type = "") {
  BV.asset_type <<- BV.asset_type

  # Title <- paste0("Percent of ", ifelse(chart_type=="CB",BondReference,"Equity")," Portfolio Value")

  browntechs <- data.frame("ald_sector" = c("Automotive", "Power", "Power", "OilGas", "OilGas", "CoalMining"), "technology" = c("ICE", "CoalCap", "GasCap", "Oil", "Gas", "Coal"))

  if (chart_type == "EQ") {
    Title <- PercentofEquityPortfolioValue #
    Batch <- filter_by_parameters(eq_peersSep, "EQ", scenario.irrelevant = T)
    combin <- filter_by_parameters(EQCombin, "EQ", scenario.irrelevant = T)
  } else if (chart_type == "CB") {
    Title <- PercentofCorporateBondsPortfolioValue #
    Batch <- filter_by_parameters(cb_peersSep, "CB", by_equity_market = FALSE, scenario.irrelevant = T)
    combin <- filter_by_parameters(CBCombin, "CB", by_equity_market = FALSE, scenario.irrelevant = T)
  } else if (chart_type == "BV") {
    if (BV.asset_type == "BV_Equity") {
      Title <- PercentofEquityPortfolioValue #
    } else if (BV.asset_type == "BV_Bonds") {
      Title <- PercentofCorporateBondsPortfolioValue #
    }
    Batch <- filter_by_parameters(BVPeersSep, "BV", by_equity_market = FALSE, scenario.irrelevant = T, BV.asset_type = BV.asset_type)
    combin <- filter_by_parameters(BVCombin, "BV", by_equity_market = FALSE, scenario.irrelevant = T, BV.asset_type = BV.asset_type)
  }

  combin <- as.data.frame(combin)
  Batch <- as.data.frame(Batch)
  # Batch <- Batch %>% filter(investor_name != investor_name_select)
  MetricCol <- VariableToPlot
  df <- bind_rows(combin, Batch)
  df <- as.data.frame(df)
  df <- subset(df, year %in% c(start_year, 2018) & ald_sector %in% c("Oil&Gas", "Coal"), select = c("investor_name", "portfolio_name", "ald_sector", "technology", MetricCol))
  df <- df[!is.na(df[, MetricCol]), ]
  if (investor_name_select %in% df$investor_name) {
    PlotChart <- TRUE
  } else {
    PlotChart <- FALSE
  }

  if (PlotChart == TRUE) {
    ID.COLS <- Grouping.Level

    BarColors <- energy
    Labels <- FossilFuelsSector #
    port.value <- sum(filter(df, portfolio_name == portfolio_name_select & investor_name == investor_name_select)[MetricCol])
    portfolio_label <- paste0(
      ThisPortfolio, " ", #
      unique(round(100 * (port.value), 2)), "%"
    )


    dfagg <- aggregate(eval(parse(text = MetricCol)) ~ eval(parse(text = ID.COLS)) + ald_sector, data = df, sum)

    colnames(dfagg) <- c("ID.COL", "ald_sector", "Metric")
    dfagg$Metric <- as.numeric(dfagg$Metric)

    dfagg <- as.data.frame(dfagg)

    dfagg <- dfagg %>%
      group_by(ID.COL) %>%
      summarise(Metricsum = sum(Metric))

    x_length <- length(unique(dfagg$ID.COL))

    xlabel <- ""
    yaxislabel <- paste0(wrap_labels(ExposureoftheportfoliotoFossilFuels, 30)) #


    if (range(dfagg$Metricsum)[1] >= 0 & range(dfagg$Metricsum)[2] >= 0) {
      lim <- c(0, range(dfagg$Metricsum)[2] + 0.005)
    } else if (range(dfagg$Metricsum)[1] < 0 & range(dfagg$Metricsum)[2] >= 0) {
      lim <- c(range(dfagg$Metricsum)[1] - 0.005, range(dfagg$Metricsum)[2] + 0.005)
    } else if (range(dfagg$Metricsum)[1] < 0 & range(dfagg$Metricsum)[2] < 0) {
      lim <- c(range(dfagg$Metricsum)[1] - 0.005, 0)
    }

    dfagg <- dfagg %>% arrange(-Metricsum)
    dfagg$ID.COL <- factor(dfagg$ID.COL, levels = dfagg$ID.COL[rev(order(dfagg$Metricsum))])

    if (ID.COLS == "portfolio_name") {
      x_coord <- which(dfagg$ID.COL == investor_name_select)
    }
    if (ID.COLS == "portfolio_name") {
      x_coord <- which(dfagg$ID.COL == portfolio_name_select)
    }

    is_left <- x_coord / x_length < .50
    is_first <- if_else(x_coord == 1, .005, 0)

    distribution_plot <- ggplot(dfagg) +
      geom_bar(aes(x = ID.COL, y = Metricsum),
        stat = "identity", width = 1
      ) +
      annotate("text",
        x = x_coord, y = port.value,
        label = portfolio_label,
        hjust = ifelse(is_left, -.05, -0.05), # 1.05
        vjust = -0.3,
        size = textsize * (5 / 14)
      ) +
      scale_fill_manual(values = BarColors, labels = Labels) + # , breaks=c(MetricC)    #
      scale_y_continuous(
        limits = lim,
        expand = expand_scale(add = c(0, 0)),
        name = yaxislabel,
        labels = percent
      ) +
      scale_x_discrete(labels = NULL, expand = c(0, 0)) +
      coord_cartesian(ylim = c(0, lim[2]), clip = "off") +
      theme_distribution() +
      geom_hline(yintercept = 0, size = 1) +
      xlab(xlabel) +
      geom_vline(xintercept = x_coord, linetype = 2) +
      theme(
        legend.position = "none",
        plot.margin = unit(c(1, 0, 0, 0), "cm")
      )


    # gt <- ggplot_gtable(ggplot_build(distribution_plot))
    # gt$layout$clip[gt$layout$name == "panel"] <- "off"
    # grid.draw(gt)
  } else {
    if (sector_to_plot == "Power") {
      SectorToInsert <- PowerSector #
    } else if (sector_to_plot == "Fossil Fuels") {
      SectorToInsert <- FossilFuelsSector #
    } else {
      SectorToInsert <- " "
    }

    Label <- paste0(Yourportfoliocontainsnoholdingsinthe, " ", SectorToInsert, " ", sectorintheselectedMarket) #
    distribution_plot <- no_chart(Label)
  }

  ggsave(plot = distribution_plot, filename = graph_name(plotnumber, ParameterFile), bg = "white", height = 2.8, width = 10, dpi = ppi)

  BV.asset_type <<- ""
}

ShippingChart <- function(plotnumber, chart_type, plot_year, sector_to_plot = "Shipping", explicit_filename = "") {

  # print(chart_type)
  # print(sector_to_plot)
  # chart_type <-"All"

  Combin <- data.frame()

  if (GraphType == "Report") {
    LegendOn <- FALSE
  } else {
    LegendOn <- TRUE
  }

  if (chart_type == "EQ" & has_equity == TRUE) {
    Peers <- filter_by_parameters(eq_peers, "EQ")
    Market <- filter_by_parameters(eq_market, "EQ")
    Combin <- filter_by_parameters(EQCombin, "EQ")
  } else if (chart_type == "CB" & has_debt == TRUE) {
    Peers <- filter_by_parameters(cb_peers, "CB", by_equity_market = FALSE)
    Market <- filter_by_parameters(cb_market, "CB", by_equity_market = FALSE)
    Combin <- filter_by_parameters(CBCombin, "CB", by_equity_market = FALSE)
  } else if (chart_type %in% c("All", "")) {
    sector_to_plot <<- "Shipping"

    Market_e <- filter_by_parameters(eq_market, "EQ")
    if (data_check(Market_e) == TRUE) {
      Market_e$Type <- "Equity Market"
    }

    Market_b <- filter_by_parameters(cb_market, "CB", by_equity_market = FALSE)
    if (data_check(Market_b) == TRUE) {
      Market_b$Type <- "Bond Market"
    }

    Combin_e <- filter_by_parameters(EQCombin, "EQ")
    if (data_check(Combin_e) == TRUE) {
      Combin_e$Type <- "Equity Portfolio"
    }

    Combin_b <- filter_by_parameters(CBCombin, "CB", by_equity_market = FALSE)
    if (data_check(Combin_b) == TRUE) {
      Combin_b$Type <- "Bond Portfolio"
    }

    if (has_equity & has_debt) {
      Market <- bind_rows(Market_e, Market_b)
    } else if (has_debt & (has_equity == FALSE)) {
      Market <- Market_b
    } else if (has_equity & (has_debt == FALSE)) {
      Market <- Market_e
    }


    if (has_equity & has_debt) {
      Combin <- bind_rows(Combin_e, Combin_b)
    } else if (has_debt & (has_equity == FALSE)) {
      Combin <- Combin_b
    } else if (has_equity & (has_debt == FALSE)) {
      Combin <- Combin_e
    }
  }

  # print(unique(Combin$ald_sector))

  if (data_check(Combin) == TRUE) {
    PlotChart <- TRUE
  } else {
    PlotChart <- FALSE
  }

  if (PlotChart == TRUE) {
    if (!chart_type %in% c("All", "")) {
      if (data_check(Peers)) {
        Peers$Type <- "Peers"
      } else {
        Type <- c()
        Type <- as.character(Type)
        Peers[, "Type"] <- Type
      }

      Peers <- unique(Peers)
      Peers <- Peers %>%
        ungroup() %>%
        filter(technology != "OilCap" & year == plot_year) %>% # year == PlotYr &
        select("portfolio_name", "ald_sector", "technology", "Type", "plan_alloc_wt_tech_prod") %>% # "Type
        rename(WtProduction = plan_alloc_wt_tech_prod)

      Peers <- Peers %>%
        complete(
          Type = c("Peers"), technology = c("A", "B", "C", "D", "E", "F", "G"),
          fill = list(WtProduction = 0, ald_sector = "Shipping")
        )


      Combin$Type <- "Portfolio"

      if (data_check(Market)) {
        Market$Type <- "Market"
      } else {
        Type <- c()
        Type <- as.character(Type)
        Market[, "Type"] <- Type
      }
    }

    Combin <- Combin %>%
      ungroup() %>%
      filter(technology != "OilCap" & year == plot_year) %>% # year == PlotYr &
      select("portfolio_name", "ald_sector", "technology", "plan_alloc_wt_tech_prod", "Type") %>% # "Type
      rename(WtProduction = plan_alloc_wt_tech_prod)

    if (chart_type %in% c("All", "")) {
      Combin <- Combin %>%
        complete(
          Type = c("Equity Portfolio", "Bond Portfolio"), technology = c("A", "B", "C", "D", "E", "F", "G"),
          fill = list(WtProduction = 0, ald_sector = "Shipping")
        )
    }


    if (chart_type %in% c("All", "") && (data_check(Market) == FALSE)) {
      Type <- c()
      Type <- as.character(Type)
      Market[, "Type"] <- Type
    }
    Market <- Market %>%
      ungroup() %>%
      filter(technology != "OilCap" & year == plot_year) %>% # year == PlotYr & Type =="Portfolio") %>%
      select("portfolio_name", "ald_sector", "technology", "plan_alloc_wt_tech_prod", "Type") %>% # "Type"
      rename(WtProduction = plan_alloc_wt_tech_prod)

    if (chart_type %in% c("All", "")) {
      Market <- Market %>%
        complete(
          Type = c("Equity Market", "Bond Market"), technology = c("A", "B", "C", "D", "E", "F", "G"),
          fill = list(WtProduction = 0, ald_sector = "Shipping")
        )
    } else {
      Market <- Market %>%
        complete(
          Type = c("Market"), technology = c("A", "B", "C", "D", "E", "F", "G"),
          fill = list(WtProduction = 0, ald_sector = "Shipping")
        )
    }


    if (!chart_type %in% c("All", "")) {
      Production <- bind_rows(Combin, Peers, Market)
    } else if (chart_type %in% c("All", "")) {
      Production <- bind_rows(Combin, Market)
    }

    # print(unique(Production$Type))

    Production$ald_sector <- as.factor(Production$ald_sector)
    levels(Production$ald_sector)[levels(Production$ald_sector) == "Coal"] <- "Fossil Fuels"
    levels(Production$ald_sector)[levels(Production$ald_sector) == "Oil&Gas"] <- "Fossil Fuels"
    # Aggregate and rename CarstenMetric_Port
    ID.COLS <- c("ald_sector", "technology", "Type")
    Production <- Production %>% gather(key = Metric, value = Value, "WtProduction")
    Production <- aggregate(Production["Value"], by = Production[c(ID.COLS)], FUN = sum)

    colours <- as.vector(ColourPalette[["Colours"]])
    names(colours) <- technology_order
    labels <- c(
      "Renewables", "Hydro", "Nuclear", "Gas", "Coal", "Electric", "Hybrid", "ICE", "Gas", "Oil", "Coal",
      "Freight", "Passenger", "A", "B", "C", "D", "E", "F", "G"
    )
    names(labels) <- technology_order
    Production$technology <- factor(Production$technology, levels = technology_order)
    Production$ald_sector <- factor(Production$ald_sector, levels = c("Fossil Fuels", "Power", "Automotive", "Aviation", "Shipping")) #
    Production$Type <- wrap_labels(Production$Type, 20)

    if (!chart_type %in% c("All", "")) {
      Production$Type <- factor(Production$Type, levels = c("Portfolio", "Peers", "Market"))
      xlabels <- c(Portf, wrap_labels(x_Peers, 10), x_Benchmark) #
    } else if (chart_type %in% c("All", "")) {
      Production$Type <- factor(Production$Type, levels = c("Bond Portfolio", "Equity Portfolio", "Bond Market", "Equity Market"))
      xlabels <- c(
        paste0(wrap_labels(BondPortfolio, 10)), #
        paste0(wrap_labels(EquityPortfollio, 10)), #
        paste0(wrap_labels(BondMarket, 10)), #
        paste0(wrap_labels(EquityMarket, 10))
      ) #
    }
    Production <- subset(Production, select = c("Type", "ald_sector", "technology", "Value"))




    shippingchart <- stacked_bar_chart(Production, colours, labels) +
      ylab(ShareofSectorProduction) + #
      scale_x_discrete(labels = xlabels, expand = c(0, 0)) +
      theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0), "cm"))

    if (LegendOn == TRUE) {
      shippingchart <- shippingchart +
        theme(
          plot.title = element_text(hjust = 0.5, colour = textcolor, size = 11, margin = unit(c(0, 0, 1, 0), "lines")),
          legend.position = "bottom",
          legend.title = element_blank()
        )
    }
  } else {
    Label <- Theportfoliohasnocompaniesintheshippingsector #
    shippingchart <- no_chart(Label)
  }
  ggsave(shippingchart, filename = graph_name(plotnumber, ParameterFile, explicit_filename = explicit_filename), bg = "white", height = 3, width = 4, dpi = ppi)

  sector_to_plot <<- ""
}

steel_chart <- function(plotnumber) {
  if (chart_type == "EQ") {
    ProdSnapshot <- filter_by_parameters(EQCompProdSnapshot, "EQ")
  }
  if (chart_type == "CB") {
    ProdSnapshot <- filter_by_parameters(CBCompProdSnapshot, "CB", by_equity_market = FALSE)
  }
  ProdSnapshot <- subset(ProdSnapshot, year = start_year + 5)
  steel <- base::merge(ProdSnapshot, SteelData, by.x = "Name", by.y = "Company") # import steel data


  nosteel <- nrow(steel)
  if (nosteel > 0) {
    # categorize high c vs. low c
    # Classification<- ifelse()
    steel <- steel %>%
      group_by(Classification) %>%
      summarize(tot = sum(tech_share)) %>%
      ungroup() %>%
      mutate(per = tech_share / tot)

    plottheme <- ggplot(
      data = steel, aes_string(x = asset_type, y = per, fill = Classification),
      show.guide = TRUE
    ) +
      geom_bar(stat = "identity", position = "fill", width = .6) +
      # geom_hline(yintercept = c(.25,.50,.75), color="white")+
      scale_fill_manual(values = colors, labels = legend_labels, breaks = names(legend_labels)) +
      scale_y_continuous(expand = c(0, 0), labels = percent) +
      guides(fill = guide_legend(nrow = 1)) +
      theme_barcharts()
  } else {
    Label <- YourPortfolioincludesnocompanieswithsteelproductionintheselectedscenario_geographyandMarket #
    outputplot <- no_chart(Label)
  }
  ggsave(filename = graph_name(plotnumber, ParameterFile), bg = "white", height = 3.6, width = 3.6, plot = outputplot, dpi = ppi)
}

SectorDataAnalysis <- function() {
  over <- subgroup_overview
  over <- subset(over, over$portfolio_name == portfolio_name_select & valid_input == 1)

  over$financial_sector <- ifelse(over$financial_sector %in% c("Coal", "Oil&Gas"), "Fossil Fuels", over$financial_sector)
  over$financial_sector <- ifelse(over$financial_sector %in% c("Aviation", "Shipping"), "Aviation & Shipping", over$financial_sector)
  over$financial_sector <- ifelse(over$financial_sector %in% c("Steel", "Cement", "Cement & Steel"), "Cement & Steel", over$financial_sector)
  over$financial_sector <- ifelse(over$financial_sector %in% c("Other"), "Not Included", over$financial_sector)

  return(over)
}

ScopeOfAnalysis <- function(plotnumber, explicit_filename = "") {
  over <- SectorDataAnalysis()

  Bond_translation <- if (BondReference == "Corporate bond") {
    paste0(wrap_labels(BondsTitle, 10)) #
  } else {
    wrap_labels(Corporate_Bond_1, 10) ## place for other category!!!
  }


  over$asset_type <- gsub("Funds", "Others", over$asset_type) #
  over$asset_type <- gsub("Bonds", Bond_translation, over$asset_type) #
  over$asset_type <- gsub("Equity", EquityTitle, over$asset_type) #
  over$asset_type <- gsub("Others", OthersTitle, over$asset_type) #


  over$Sector.All <- ifelse(over$financial_sector == "Not Included", "Sectors Not in Scope", "Scope of the Analysis")

  over <- over %>%
    ungroup() %>%
    select(Sector.All, asset_type, valid_value_usd) %>%
    group_by(Sector.All, asset_type) %>%
    summarise(valid_value_usd = sum(valid_value_usd))


  if (data_check(over) == F) {
    over <- over %>%
      ungroup() %>%
      add_row(Sector.All = "Power", asset_type = Bond_translation, valid_value_usd = 0)
  }

  over <- over %>%
    complete(
      asset_type = c(Bond_translation, EquityTitle, OthersTitle), #
      Sector.All = c(
        "Sectors Not in Scope",
        # "Other Climate Sectors",
        "Scope of the Analysis"
      ),
      fill = list(valid_value_usd = 0)
    ) %>%
    unique()

  over <- as.data.frame(over)
  orderofchart <- c(Bond_translation, EquityTitle, OthersTitle) #
  over$asset_type <- factor(over$asset_type, levels = orderofchart)
  over$Sector.All <- factor(over$Sector.All, levels = c(
    "Sectors Not in Scope",
    # "Other Climate Sectors",
    "Scope of the Analysis"
  ), ordered = TRUE)
  tot <- over %>%
    group_by(asset_type) %>%
    summarise(s = sum(valid_value_usd))

  # for NEW VERSION horizontal chart
  over_horizontal_chart <- over %>% filter(asset_type != OthersTitle)

  annotate.position <- over_horizontal_chart %>%
    group_by(asset_type) %>%
    summarize(sumValueUSD = sum(valid_value_usd)) %>%
    summarize(maxsumValueUSD = max(sumValueUSD))

  annotate.label <- over_horizontal_chart %>% filter(Sector.All == "Scope of the Analysis")

  number_cifer <- nchar(round(annotate.position$maxsumValueUSD, digits = 0))

  breaks_by <- function(number_cifer) {
    10^(number_cifer - 1)
  }
  breaks_interval <- breaks_by(number_cifer)

  plot <- ggplot(over_horizontal_chart, aes(x = fct_rev(asset_type), y = valid_value_usd, fill = Sector.All)) +
    geom_bar(position = "stack", stat = "identity", width = 0.9) +
    coord_flip() +
    scale_fill_manual(
      name = "", labels = c(
        SectorsNotinScope,
        # OtherClimateRelevantSectors,
        SectorsinScenarioAnalysis
      ),
      values = c(
        "grey80",
        # "#deebf7",
        "#265b9b"
      ), drop = FALSE
    ) + #
    # scale_x_discrete(name="Asset Type", labels = x_labels) +
    scale_y_continuous(
      name = MarketValue, labels = comprss, expand = c(0, 0, 0, 0),
      breaks = seq(0, annotate.position$maxsumValueUSD, by = breaks_interval * 2)
    ) + # breaks = scales::pretty_breaks(n = 3)) +   #
    # limits = c(0, annotate.position$maxsumValueUSD+0.3*annotate.position$maxsumValueUSD)) +     #
    guides(fill = guide_legend(nrow = 1, byrow = TRUE, reverse = TRUE)) +
    theme_barcharts() +
    theme(
      plot.title = element_text(colour = "#265b9b", size = 11, hjust = 1),
      legend.position = "bottom",
      legend.text = element_text(size = textsize),
      legend.box.margin = unit(c(-15, 60, -10, 0), "pt"),
      legend.spacing.y = unit(0, "pt"),
      axis.line.x = element_blank(),
      axis.line.y = element_line(size = 0.5),
      axis.text.x = element_text(colour = textcolor, size = 11),
      axis.text.y = element_text(colour = textcolor, size = 11),
      axis.title.x = element_text(colour = textcolor, size = 11, margin = margin(5, 0, 0, 0, "pt"), hjust = 0.3),
      axis.title.y = element_blank(),
      panel.grid.major.x = element_line(colour = "#989898", size = 0.2),
      plot.margin = unit(c(5, 10, 10, 2), "pt")
    ) +
    annotate("text",
      x = annotate.label$asset_type, y = annotate.position$maxsumValueUSD + 0.35 * annotate.position$maxsumValueUSD,
      # label = wrap_labels(paste0(comma(round(annotate.label$valid_value_usd, digits = 0))), 10),
      label = comprss(annotate.label$valid_value_usd),
      family = textfont, colour = "#265b9b", hjust = 1
    ) +
    ggtitle(Climaterelevantsectors)


  ggsave(plot, filename = graph_name(plotnumber, ParameterFile, explicit_filename = explicit_filename), bg = "white", height = 2.2, width = 4.5, dpi = ppi) # linewidth_in*.9
}

CarstenMetricChart <- function(plotnumber, chart_type, explicit_filename = "") {
  PortName_IN <- portfolio_name_select

  if (chart_type == "CB" & has_debt == TRUE) {
    market <- filter_by_parameters(cb_market, "CB", by_equity_market = FALSE)
    combin <- filter_by_parameters(CBCombin, "CB", by_equity_market = FALSE)

    port <- bind_rows(combin, market)

    Type <- "Corporate bond"
    # FIXME: Replace with dplyr::recode() or other options
    port$portfolio_name <- plyr::mapvalues(port$portfolio_name, c(portfolio_name_select, cb_market_ref), c("Portfolio", cb_market_ref))

    port$portfolio_name <- factor(port$portfolio_name, levels = c("Portfolio", cb_market_ref), ordered = TRUE)
    lab <- wrap_labels(cb_market_ref, 12)
    MarketRef <- cb_market_ref
  } else if (chart_type == "CB" & has_debt == FALSE) {
    PlotChart <- FALSE
  }

  if (chart_type == "EQ" & has_equity == TRUE) {
    # Set as CB to ensure that the PortWeight values are used
    market <- filter_by_parameters(eq_market, "CB", scenario.irrelevant = TRUE)
    port <- filter_by_parameters(EQCombin, "CB", scenario.irrelevant = TRUE)

    port <- bind_rows(port, market)
    Type <- "equity"

    port <- subset(port, portfolio_name %in% c(portfolio_name_select, eq_market_ref)) # "ListedEquity"
    # FIXME: Replace with dplyr::recode() or other options
    port$portfolio_name <- plyr::mapvalues(port$portfolio_name, c(portfolio_name_select, eq_market_ref), c("Portfolio", eq_market_ref)) # "ListedEquity"
    port$portfolio_name <- factor(port$portfolio_name, levels = c("Portfolio", eq_market_ref), ordered = TRUE)
    lab <- wrap_labels(eq_market_ref, 10)

    MarketRef <- eq_market_ref
  } else if (chart_type == "EQ" & has_equity == FALSE) {
    PlotChart <- FALSE
  }



  if (PlotChart == TRUE) {
    # print("Plotting preparation")

    current.port <- subset(port, year == start_year & portfolio_name == "Portfolio") %>%
      mutate(Metric = plan_carsten)
    current.market <- subset(port, year == start_year & portfolio_name %in% c(eq_market_ref, cb_market_ref)) %>%
      mutate(Metric = plan_carsten)


    port <- bind_rows(current.port, current.market) %>% select(portfolio_name, ald_sector, technology, Metric)

    sector_list <- sector_list[sector_list != "Fossil Fuels"]
    tech_list <- tech_list[tech_list != "OilCap"]

    port <- port %>%
      complete(
        portfolio_name = c("Portfolio", MarketRef),
        technology = tech_list,
        fill = list(Metric = 0)
      )

    port <- port %>% mutate( # portfolio_name = if_else(investor_name == investor_name_select, portfolio_name_select, MarketRef),
      ald_sector = set_ald_sector(technology)
    )



    port$ald_sector2 <- paste0(port$ald_sector, " Production") #
    port$ald_sector2 <- ifelse(port$ald_sector == "Power", C_Power, port$ald_sector2) #
    port$ald_sector2 <- ifelse(port$ald_sector == "Automotive", S_Automotive, port$ald_sector2) #
    port$ald_sector2 <- ifelse(port$ald_sector == "Oil&Gas", P_OilandGas, port$ald_sector2) #
    port$ald_sector2 <- ifelse(port$ald_sector == "Coal", P_Coal, port$ald_sector2) #
    port$ald_sector2 <- factor(port$ald_sector2, levels = c(P_OilandGas, P_Coal, C_Power, S_Automotive)) # levels OK
    port <- subset(port, technology != "OilCap")
    tech.levels <- technology_order_short

    tech.labels <- c(
      P_Gas, P_Oil, P_Coal, #
      C_Renewables, C_Hydro, C_Nuclear, C_Gas, C_Coal, #
      T_Electric, T_Hybrid, T_ICE
    ) #

    if (max(port$Metric) <= 0.001) {
      perc.labels.digits <- 0.01
    } else if (max(port$Metric) <= 0.01) {
      perc.labels.digits <- 0.1
    } else {
      perc.labels.digits <- 1
    }


    port$technology <- factor(port$technology, levels = tech.levels, ordered = TRUE)

    port$portfolio_name <- dplyr::recode(port$portfolio_name,
      "Portfolio" = Portf,
      MarketRef = lab
    )

    port$portfolio_name <- factor(port$portfolio_name, levels = c(Portf, MarketRef), ordered = TRUE)

    tech.colors <- c(GasProdColour, OilProdColour, CoalProdColour, RenewablesColour, HydroColour, NuclearColour, GasCapColour, CoalCapColour, ElectricColour, HybridColour, ICEColour)
    tots <- port %>%
      group_by(portfolio_name, ald_sector2) %>%
      summarise(Metric = sum(Metric, na.rm = T))



    outputplot <- ggplot(port, aes(x = portfolio_name, y = Metric, group = technology, fill = technology)) +
      geom_bar(stat = "identity", position = "stack") +
      # geom_text(data=tots, aes(x=PortName, y=Metric, label=percent(Metric))) +
      scale_x_discrete(name = "", labels = c(Portf, lab)) + #
      scale_y_continuous(
        name = paste0(wrap_labels(Weightbymarketvalueofissuersexposedtothetechnology, 50), "\n"), labels = percent_format(accuracy = perc.labels.digits), expand = c(0, 0), #
        limits = c(0, max(tots$Metric) + .01)
      ) +
      scale_fill_manual(name = tech.levels, labels = tech.labels, values = tech.colors) +
      guides(fill = guide_legend(ncol = 1)) +
      theme_cdi() +
      facet_wrap(~ald_sector2, nrow = 1, labeller = label_wrap_gen(width = 20)) +
      theme(
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 11),
        axis.text.x = element_text(angle = 0, colour = textcolor, size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 13),
        strip.text = element_text(size = 13),
        # axis.ticks.y = element_line(colour=textcolor,size = 1),
        axis.line.x = element_line()
      )
  } else {
    Label <- paste(Nodataintheportfolio) #
    outputplot <- no_chart(Label)
  }

  ggsave(outputplot, filename = graph_name(plotnumber, ParameterFile, explicit_filename = explicit_filename), bg = "white", height = 4, width = 13, dpi = ppi * 0.8)

  BV.asset_type <<- ""
}

RenewableAdditionsChart <- function(plotnumber, companiestoprint, chart_type) {
  sector_to_plot <<- "Power"
  RenewableAdds <- NA

  RenewableAdditions <- readRDS(paste0(Location, "ReferenceData/RenewablesAdditionsData.rda"))
  RenewableAdditions$ald_sector <- "Power"
  # RenewableAdditions$scenario_geography <- ifelse(RenewableAdditions$ald_sector == "Power","Global",RenewableAdditions$scenario_geography)


  if (chart_type == "EQ") {
    RenewableAdditions <- filter_by_parameters(RenewableAdditions, "EQ") %>%
      filter(FI %in% c("Equity", "EQUITY"))
    CompProdSnapshot <- filter_by_parameters(EQCompProdSnapshot, "EQ", ActorSectorOnly = T)

    RenewableAdds <- RenewableAdditions[RenewableAdditions$ID %in% CompProdSnapshot$bloomberg_id, ]
  }
  if (chart_type == "CB") {
    RenewableAdditions <- filter_by_parameters(RenewableAdditions, "CB") %>%
      filter(FI %in% c("Bonds", "BONDS"))
    CompProdSnapshot <- filter_by_parameters(CBCompProdSnapshot, "CB", by_equity_market = F, ActorSectorOnly = T)
    RenewableAdds <- RenewableAdditions[RenewableAdditions$ID %in% CompProdSnapshot$corp_bond_ticker, ]
  }

  if (data_check(RenewableAdds) == T) {
    PlotChart <- TRUE
  } else {
    PlotChart <- FALSE
  }


  if (PlotChart == TRUE) {
    RenewablesBar <- RenewableAdds
    maxcompanies <- min(companiestoprint, nrow(RenewablesBar))

    RenewablesBar <- RenewablesBar[order(RenewablesBar$plan_tech_prod, decreasing = T), ]
    # RenewablesBar$company_name <- factor(RenewablesBar$company_name, levels = RenewablesBar$company_name[order(RenewablesBar$Additions)])

    RenewablesBar <- RenewablesBar[seq(1, maxcompanies), ]


    Yaxislabel <- paste0(Renewablecapacityadditions, " (", start_year, " - ", start_year + 5, ")") #
    stillreq <- " Remaining Required Additions" # not used?
    remainlabel <- x_Progress #
    progresslabel <- RemainingRequiredAdditions #
    Target <- "Progress" # not used?

    RenewablesBar <- RenewablesBar %>% select(company_name, Additions, RequiredAdditions)
    RenewablesBarLong <- melt(RenewablesBar, variable.name = "variable", value.name = "value", id.vars = c("company_name"))

    sumren <- RenewablesBarLong %>%
      group_by(company_name) %>%
      summarize(tot = sum(value))
    RenewablesBarLong <- left_join(RenewablesBarLong, sumren, by = "company_name") %>%
      mutate(perc = value / tot)
    RenewablesBarLong$variable <- factor(RenewablesBarLong$variable, levels = c("RequiredAdditions", "Additions"))

    RenewablesBarLong <- RenewablesBarLong %>%
      filter(company_name %in% unique(RenewablesBarLong$company_name)[1:min(companiestoprint, length(unique(RenewablesBarLong$company_name)))])

    comp <- subset(RenewablesBarLong, variable == "RequiredAdditions")

    company_labels <- trimws(unique(RenewablesBarLong$company_name))
    for (i in 1:length(company_labels)) {
      if (str_length(company_labels[i]) > 15) {
        new_name <- strtrim(company_labels[i], 15)
        company_labels[i] <- paste0(new_name, "...")
      }
    }

    CompName <- RenewablesBarLong[order(RenewablesBarLong[which(RenewablesBarLong$variable == "Additions"), ]$perc, decreasing = T), ]$company_name

    RenewablesBarLong$company_name <- factor(RenewablesBarLong$company_name, levels = rev(CompName))

    RenAddBar <- ggplot(RenewablesBarLong, aes(x = company_name, y = perc, fill = variable)) +
      geom_bar(stat = "identity", width = .8, position = "fill") +
      # geom_segment(aes(x=0, xend = 0 , y=0, yend = 1), size=1, colour = AxisColour,  arrow = arrow(length = unit(0.4,"cm")))+
      geom_hline(yintercept = 1, colour = area_2, linetype = "longdash", size = 1) +
      scale_fill_manual(values = c("RequiredAdditions" = badexpColour, "Additions" = YourportColour), labels = c("Additions" = remainlabel, "RequiredAdditions" = progresslabel)) +
      scale_y_continuous(limits = c(0, 1), expand = c(-0.1, 0.1), label = percent) +
      scale_x_discrete(expand = c(0, 0), label = company_labels) +
      theme_barcharts() +
      guides(fill = guide_legend(nrow = 1, reverse = TRUE)) +
      coord_flip() +
      theme(
        axis.title.x = element_text(colour = AxisColour, size = 11),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 11),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size = 11, colour = AxisColour, margin = margin(l = 0.15, r = 0.35, unit = "cm")),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.key.size = unit(0.4, "cm"),
        legend.title = element_blank(),
        legend.key = element_blank(),
        plot.margin = unit(c(.4, 1.5, 0, 0), "lines")
      ) +
      ylab(Yaxislabel) +
      geom_text(
        data = comp,
        aes(x = company_name, y = 0.88, hjust = 1),
        label = paste0(round(comp$value, digits = 0), " MW ", RemainingRequiredAdditions), color = textcolor, size = 3
      ) # 2.53    #

    RenAddValues <- c(4.8, 8)

    bar_size <- 8 / 15

    height <- min(1 + companiestoprint, n_distinct(RenewablesBarLong$company_name) / 2) * bar_size + .86

    # plotwidth <- .1*NoCompanies +7.1
  } else {
    Label <- YourPortfoliodoesnotincludeanycompanieswithrenewablepower #
    RenAddBar <- no_chart(Label)

    # plotwidth <- 7
    height <- 3
  }
  ggsave(filename = graph_name(plotnumber, ParameterFile), height = height, width = 10, plot = RenAddBar, dpi = ppi)
  sector_to_plot <<- ""
}

carboninoutdata <- function(chart_type) {
  ccap <- NA
  portfolio1 <- NA

  if (chart_type == "EQ") {
    EQCompProdSnapshot <- filter_by_parameters(EQCompProdSnapshot, "EQ")

    if (data_check(EQCompProdSnapshot) == TRUE) {
      CompProdSS <- EQCompProdSnapshot %>%
        ungroup() %>%
        filter(year == start_year + 5) %>%
        select(company_name, port_weight, id) %>%
        rename(Name = company_name) %>%
        unique()
      CompProdSS <- as.data.frame(CompProdSS)
      CompProdSS$id <- as.integer(CompProdSS$id)
      ccap <- CarbonData
    } else {
      PlotChart <- FALSE
    }
  } else if (chart_type == "CB") {
    CBCompProdSnapshot <- filter_by_parameters(CBCompProdSnapshot, "CB", by_equity_market = FALSE)
    if (data_check(CBCompProdSnapshot) == TRUE) {
      CompProdSS <- CBCompProdSnapshot %>%
        filter(year == start_year + 5) %>%
        select(port_weight, corp_bond_ticker) %>%
        rename(EQY_FUND_TICKER = corp_bond_ticker) %>%
        unique()
    } else {
      PlotChart <- FALSE
    }

    ccap <- CarbonData
    ccap$EQY_FUND_TICKER <- gsub(" [A-z ]*", "", as.character(ccap$EQY_FUND_TICKER))
  }

  if (data_check(ccap) == TRUE) {
    ccap$InsideCarbonBudget <- ccap$TotalCarbonBudget - ccap$OutsideCarbonBudget
    ccap <- subset(ccap, select = c("id", "TotalCarbonBudget", "OutsideCarbonBudget", "InsideCarbonBudget"))

    colnames(ccap) <- c("id", "TotalCarbonBudget", "Outside Carbon Budget", "Inside Carbon Budget")
    ccap$Inside.Carbon.Budget <- ccap$`Inside Carbon Budget` / ccap$TotalCarbonBudget
    ccap$Outside.Carbon.Budget <- ccap$`Outside Carbon Budget` / ccap$TotalCarbonBudget

    portfolio <- left_join(CompProdSS, ccap, by = "id") %>%
      select(Name, port_weight, Inside.Carbon.Budget, Outside.Carbon.Budget)

    portfolio1 <- melt(portfolio, id.vars = c("Name", "port_weight"), variable.name = "CarbonBudget")
    portfolio1 <- subset(portfolio1, !is.na(value))

    if (data_check(portfolio1) == TRUE) {
      HasCarbonBudget <<- TRUE
    } else {
      HasCarbonBudget <<- FALSE
    }
  } else {
    HasCarbonBudget <<- FALSE
  }

  return(portfolio1)
}

CarbonBudget <- function(plotnumber, companiestoprint, chart_type) {
  portfolio1 <- carboninoutdata(chart_type)

  if (HasCarbonBudget == TRUE) {
    carbonorder <- c("Inside Carbon Budget", "Outside Carbon Budget")

    portfolio1$CarbonBudget <- gsub("\\.", " ", portfolio1$CarbonBudget)

    colors <- c(OilProdColour, area_6)

    # AllData <- filter(AllData, technology %in% techorder)
    portfolio1$CarbonBudget <- factor(portfolio1$CarbonBudget, levels = carbonorder)

    portfolio1 <- portfolio1 %>%
      arrange(-port_weight)

    portfolio1 <- portfolio1 %>%
      filter(Name %in% unique(portfolio1$Name)[1:min(companiestoprint, length(unique(portfolio1$Name)))])


    dummy <- data.frame(
      c("Name", ""),
      c("port_weight", NA),
      c("CarbonBudget", NA),
      c("value", 0)
    )
    colnames(dummy) <- as.character(unlist(dummy[1, ]))
    dummy <- dummy[-1, ]
    dummy$value <- as.numeric(dummy$value)

    portfolio1 <- rbind(portfolio1, dummy)
    # portfolio1$Name <- factor(portfolio1$Name, levels=rev(unique(c("",portfolio1$Name))))
    portfolio1$port_weight <- as.numeric(portfolio1$port_weight)


    portfolio1 <- portfolio1 %>%
      arrange(port_weight)


    company_labels <- trimws(unique(portfolio1[!is.na(portfolio1$CarbonBudget), ]$Name))
    for (i in 1:length(company_labels)) {
      if (str_length(company_labels[i]) > 15) {
        new_name <- strtrim(company_labels[i], 15)
        company_labels[i] <- paste0(new_name, "...")
      }
    }

    portfolio1$CarbonBudget <- recode(portfolio1$CarbonBudget,
      "Outside Carbon Budget" = OutsideCarbonBudget,
      "Inside Carbon Budget" = InsideCarbonBudget
    )
    portfolio1$CarbonBudget <- factor(portfolio1$CarbonBudget, levels = c(OutsideCarbonBudget, InsideCarbonBudget))


    bar_labels <- c(company_labels, "                          ")
    portfolio1$port_weight <- as.numeric(portfolio1$port_weight)
    carb <- subset(portfolio1, !is.na(port_weight))
    carbonorder <- recode(carbonorder,
      "Outside Carbon Budget" = OutsideCarbonBudget,
      "Inside Carbon Budget" = InsideCarbonBudget
    )
    names(colors) <- carbonorder

    PortPlot <- ggplot(
      data = portfolio1, aes(
        x = reorder(Name, port_weight), y = value,
        fill = factor(CarbonBudget, levels = c(OutsideCarbonBudget, InsideCarbonBudget))
      ),
      show.guide = TRUE
    ) +
      geom_bar(stat = "identity", position = "fill", width = .6) +
      # geom_hline(yintercept = c(.25,.50,.75), color="white")+
      scale_fill_manual(values = colors, labels = paste(carbonorder, " "), breaks = (carbonorder)) +
      scale_y_continuous(expand = c(0, 0), labels = percent) +
      scale_x_discrete(labels = bar_labels) +
      guides(fill = guide_legend(nrow = 1)) +
      theme_barcharts() +
      geom_text(
        data = carb, aes(x = Name, y = 1),
        label = perc(carb$port_weight),
        hjust = -1, color = textcolor, size = textsize * (5 / 14)
      ) +
      geom_text(aes(x = "", y = 1),
        label = x_Weight, #
        hjust = -0.5, color = textcolor, size = 10 * (5 / 14),
        family = textfont
      ) +
      xlab("") +
      ylab("tech_share") +
      coord_flip() +
      theme(
        legend.position = "bottom", legend.title = element_blank(),
        plot.margin = unit(c(1, 6, 0, 0), "lines"), axis.line.x = element_line(colour = textcolor, size = 0.5)
      ) #+
    #   annotation_custom(
    #     grob = textGrob(label = x_Weight,
    #                     gp=gpar(fontsize=8.5),
    #                     hjust = 0),
    #     xmin = n_distinct(portfolio1$Name)+0.5, xmax = n_distinct(portfolio1$Name)+1, ymin = 1, ymax = 1.05)
    #
    # # gt <- PortPlot

    gt <- ggplot_gtable(ggplot_build(PortPlot))
    gt$layout$clip[gt$layout$name == "panel"] <- "off"

    dev.off()
    # grid.draw(gt)

    bar_size <- 4 / 15

    h <- min(1 + companiestoprint, n_distinct(portfolio1$Name)) * bar_size + .66

    HasCarbonBudget <<- TRUE
  } else {
    Label <- NoCarbonBudgetDataisavailablefortheOilCompaniesinyourportfolio #
    # Label <- "No results are available for this combination of parameters"
    gt <- no_chart(Label)

    h <- 3

    HasCarbonBudget <<- FALSE
  }
  ggsave(plot = gt, filename = graph_name(plotnumber, ParameterFile), bg = "white", height = h, width = 11, dpi = ppi)
}

OilGasBuildOut <- function(plotnumber, companiestoprint, chart_type, explicit_filename = "") {
  sector_to_plot <<- "Oil&Gas"
  # chart_type <<- chart_type

  if (chart_type == "CB") {
    comp <- filter_by_parameters(CBCompProdSnapshot, "CB", by_equity_market = F)

    if (data_check(comp) == TRUE) {
      comp <- subset(comp, portfolio_name == portfolio_name_select & ald_sector == "Oil&Gas")
      comp$company_name <- comp$id

      port <- filter_by_parameters(CBCombin, "CB", by_equity_market = F)
      port.targets <- port %>%
        filter(ald_sector == "Oil&Gas") %>%
        select(investor_name, portfolio_name, scenario, allocation, equity_market, scenario_geography, ald_sector, technology, scen_tech_prod, year) %>%
        arrange(year) %>%
        group_by(investor_name, portfolio_name, scenario, allocation, equity_market, scenario_geography, ald_sector, technology) %>%
        summarise(
          Port.Scen.Diff = last(scen_tech_prod) - first(scen_tech_prod),
          Port.Scen.Pct = Port.Scen.Diff / first(scen_tech_prod)
        ) %>%
        ungroup() %>%
        select(-scenario, -allocation, -equity_market, -scenario_geography, -ald_sector)
    }
  }
  if (chart_type == "EQ") {
    comp <- filter_by_parameters(EQCompProdSnapshot, "EQ")

    if (data_check(comp) == TRUE) {
      comp <- subset(comp, portfolio_name == portfolio_name_select & ald_sector == "Oil&Gas" & year %in% c(start_year, start_year + 5))

      port <- filter_by_parameters(EQCombin, "EQ")
      port <- subset(port, year %in% c(start_year, start_year + 5))
      port.targets <- port %>%
        filter(ald_sector == "Oil&Gas") %>%
        select(
          investor_name, portfolio_name, scenario, allocation, equity_market, scenario_geography,
          ald_sector, technology, scen_tech_prod, year
        ) %>%
        arrange(year) %>%
        group_by(investor_name, portfolio_name, scenario, allocation, equity_market, scenario_geography, ald_sector, technology) %>%
        summarise(
          Port.Scen.Diff = last(scen_tech_prod) - first(scen_tech_prod),
          Port.Scen.Pct = Port.Scen.Diff / first(scen_tech_prod)
        ) %>%
        ungroup() %>%
        select(-scenario, -allocation, -equity_market, -scenario_geography, -ald_sector)
    }
  }

  if (data_check(comp) == TRUE) {
    comp <- comp %>%
      filter(ald_sector == "Oil&Gas") %>%
      ungroup() %>%
      select(scenario, company_name, ald_sector, technology, plan_tech_prod, scen_tech_prod, year) %>%
      arrange(scenario, company_name, ald_sector, technology, year) %>%
      group_by(scenario, company_name, ald_sector, technology) %>%
      filter(plan_tech_prod > 0) %>%
      summarise(first(plan_tech_prod), last(plan_tech_prod),
        Plan.Diff = last(plan_tech_prod) - first(plan_tech_prod),
        Scen.Diff = last(scen_tech_prod) - first(scen_tech_prod),
        Plan.Pct = ifelse(Plan.Diff == 0, 0, (last(plan_tech_prod) - first(plan_tech_prod)) / first(plan_tech_prod))
      )
  }

  ## TODO: FIXME, THIS IS A HACK FOR THE SFC PROJECT
  comp <- comp %>% mutate(Plan.Pct = ifelse(company_name == "PETRPE", (2347 - 1250) / 1250, Plan.Pct))

  if (data_check(filter(comp, company_name == "PETRPE"))) {
    tmp_row <- filter(comp, company_name == "PETRPE")
    tmp_row <- tmp_row %>% mutate(technology = "Gas", Plan.Pct = 0)

    comp <- rbind(comp, tmp_row)
  }
  ## TODO: END FIXME, THIS IS A HACK FOR THE SFC PROJECT


  if (data_check(comp) == TRUE) {
    comp <- left_join(comp, port.targets %>% ungroup() %>% as.data.frame(), by = "technology")
    comp$technology <- factor(comp$technology, levels = c("Gas", "Oil"), ordered = TRUE)
    comp$technology <- dplyr::recode(comp$technology, Gas = T_GasProd, Oil = T_OilProd) #
    comp <- comp %>%
      group_by(scenario, technology) %>%
      top_n(wt = `first(plan_tech_prod)`, n = companiestoprint)

    # Seperate out the infinite values if necessary
    comp_inf <- comp[is.infinite(comp$Plan.Pct), ]
    comp <- comp[is.finite(comp$Plan.Pct), ]


    MAX.VAL <- max(abs(comp$Plan.Pct), na.rm = TRUE)
    temp.max <- ceiling(MAX.VAL * 10) / 10 + 0.2
    temp.diff <- ceiling(temp.max * 2) / 10

    breaks <- seq(0, temp.max + temp.diff, temp.diff)
    breaks <- unlist(c(rev(-1 * breaks), breaks))

    limits <- c(min(breaks), max(breaks))

    comp$InfFlag <- 0

    if (data_check(comp_inf) == TRUE) {
      comp_inf$Plan.Pct <- MAX.VAL * 1
      comp_inf$InfFlag <- 1
      comp <- rbind(comp, comp_inf)
    }

    comp$Ord.Var <- paste0(comp$company_name, comp$technology)
    comp <- comp %>%
      ungroup() %>%
      arrange(`first(plan_tech_prod)`)
    comp$Ord.Var <- factor(comp$Ord.Var, levels = comp$Ord.Var, ordered = TRUE)

    comp$Final.Name <- shortened_company_names(comp$company_name, 15)
    port.targets$technology <- dplyr::recode(port.targets$technology, Gas = T_GasProd, Oil = T_OilProd) #

    outputplot <- ggplot(comp, aes(x = Ord.Var, y = Plan.Pct, fill = technology)) +
      geom_bar(stat = "identity") +
      geom_hline(
        data = port.targets, aes(
          yintercept = Port.Scen.Pct,
          linetype = paste0(ChangeinPortfolioProductionSpecifiedby, " ", Scenariochoose, " ", Scenariofrom, " ", start_year, "- ", start_year + 5)
        ), #
        color = area_2, size = 1.5
      ) +
      coord_flip() +
      scale_x_discrete(name = "", labels = setNames(comp$Final.Name, as.character(comp$Ord.Var))) +
      scale_y_continuous(
        name = paste0(ChangeinPlannedProductionfrom, " ", start_year, "- ", start_year + 5), #
        labels = percent_format(accuracy = 1), breaks = round(breaks, digits = 2), limits = limits
      ) +
      scale_color_manual(values = area_2) +
      scale_linetype_manual(name = "", values = c("solid")) +
      scale_fill_manual(name = "", values = c(OilProdColour, GasProdColour)) + # guide_legend(reverse = TRUE)
      facet_wrap(~technology, ncol = 1, scales = "free_y") +
      theme_cdi() +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 11)
      ) +
      theme(axis.line = element_line()) +
      theme(axis.ticks.x = element_line())

    # Adds the infinite label
    if (data_check(comp_inf) == TRUE) {
      outputplot <- outputplot +
        geom_text(data = comp[comp$InfFlag == 1, ], aes(x = Ord.Var, y = 0, label = "Future Production Only - Infinite Build Out", hjust = 0))
      # annotate("text", x= 5, y=0, label = "Future Production Only - Infinite Build Out", hjust = 0)
    }


    bar_size <- 6 / 15
    h <- min(1 + companiestoprint, n_distinct(comp$company_name)) * bar_size + 1.5
  } else {
    Label <- Nocompanieswithexposuretoeitheroilorgasproductioninyourportfolio #
    outputplot <- no_chart(Label)
    h <- 3.25
  }
  ggsave(outputplot,
    filename = graph_name(plotnumber, ParameterFile, explicit_filename = explicit_filename),
    height = h, width = 10, dpi = ppi
  )

  sector_to_plot <<- ""
  chart_type <<- ""
  BV.asset_type <<- ""
}

CoalRetirementChart <- function(plotnumber, companiestoprint, chart_type) {
  PlotChart <- F
  chart_type <- chart_type
  sector_to_plot <<- "Power"
  PortCoalRetirements <- NA

  CoalRetirements$equity_market <- gsub("Market", "", CoalRetirements$equity_market)
  # CoalRetirements$scenario_geography <- ifelse(CoalRetirements$scenario_geography == "Global","GlobalAggregate",CoalRetirements$scenario_geography)
  CoalRetirements$ald_sector <- "Power"

  if (chart_type == "EQ" & has_equity) {
    CoalRet <- filter_by_parameters(CoalRetirements, "EQ") %>%
      filter(FI %in% c("Equity", "EQUITY"))

    if ("ALD.ID" %in% colnames(CoalRet)) {
      CoalRet <- CoalRet %>% rename(ID = ALD.ID)
    }
    PortProduction <- filter_by_parameters(EQCompProdSnapshot, "EQ") # %>%
    # filter(technology == "CoalCap")
  } else if (chart_type == "CB" & has_debt) {
    CoalRet <- filter_by_parameters(CoalRetirements, "CB", by_equity_market = F) %>%
      filter(FI %in% c("Bonds", "BONDS"))
    if ("ALD.ID" %in% colnames(CoalRet)) {
      CoalRet <- CoalRet %>% rename(ID = ALD.ID)
    }
    PortProduction <- filter_by_parameters(CBCompProdSnapshot, "CB", by_equity_market = F)
  }

  if (data_check(PortProduction)) {
    PortCoalRetirements <- PortProduction %>% filter(technology == "CoalCap")
  }

  if (data_check(PortCoalRetirements)) {
    if (chart_type == "EQ") {
      PortCoalRetirements <- CoalRet %>%
        filter(ID %in% PortProduction$bloomberg_id)
    } else if (chart_type == "CB") {
      PortCoalRetirements <- CoalRet %>%
        filter(ID %in% PortProduction$corp_bond_ticker)
    }
  }

  if (data_check(PortCoalRetirements)) {
    PlotChart <- T
  }


  if (PlotChart == T) {
    PortCoalRetirements <- as.data.frame(PortCoalRetirements)
    PortCoalRet <- PortCoalRetirements %>%
      select(company_name, ExcessiveAdditions, RequiredRetirements10yr, PctRetire) %>%
      mutate(Total = ExcessiveAdditions + RequiredRetirements10yr)

    PortCoalRet$company_nameShort <- shortened_company_names(PortCoalRet$company_name, 15)

    PortCoalRet <- PortCoalRet[order(PortCoalRet$Total, decreasing = T), ]
    PortCoalRet <- PortCoalRet[PortCoalRet$Total > 0, ]

    maxcompanies <- min(companiestoprint, nrow(PortCoalRet))
    PortCoalRet <- PortCoalRet[seq(1, maxcompanies), ]

    PortCoalRet$company_name <- factor(PortCoalRet$company_name, levels = unique(PortCoalRet$company_name[order(PortCoalRet$Total)]))

    CoalRetmaxxAxis <- max(PortCoalRet$Total)

    PortCoalRet <- PortCoalRet[!is.na(PortCoalRet$ExcessiveAdditions), ]

    PlotChart <- data_check(PortCoalRet)
  }

  if (PlotChart == T) {
    PortCoalRet <- PortCoalRet %>% select(-Total, -PctRetire)
    PortCoalRetLong <- melt(PortCoalRet, variable.name = "variable", value.name = "value", id.vars = c("company_name", "company_nameShort"))

    RetUnit <- "MW"

    if (CoalRetmaxxAxis > 1000) {
      PortCoalRetLong$value <- PortCoalRetLong$value / 1000
      CoalRetmaxxAxis <- ceiling(CoalRetmaxxAxis / 1000)
      RetUnit <- "GW"
    }

    CoalRetmaxxAxis <- round(CoalRetmaxxAxis, digits = 1)

    outputplot <- ggplot(PortCoalRetLong, aes(x = company_name, y = value, fill = variable)) +
      geom_bar(stat = "identity", width = .8, colour = NA) +
      scale_fill_manual(
        values = c("ExcessiveAdditions" = "#170203", "RequiredRetirements10yr" = "#ed1c24"),
        labels = c(
          paste0(AdditionalCapacityby, " ", start_year + 10), #
          paste0(RequiredRetirementsby, " ", start_year + 10)
        ), name = "year"
      ) + #
      scale_y_continuous(
        expand = c(0, 0), limits = c(0, CoalRetmaxxAxis * 1.05), breaks = seq(0, CoalRetmaxxAxis, CoalRetmaxxAxis * 0.1),
        minor_breaks = seq(0, CoalRetmaxxAxis, CoalRetmaxxAxis * 0.05)
      ) +
      scale_x_discrete(expand = c(0, 0), labels = rev(PortCoalRetLong$company_nameShort)) +
      xlab("") +
      ylab(paste0(Requiredcoalretirementspercompany, " (", RetUnit, ")")) + #
      # annotate(hjust = 0.8,"text", x = c(1:(length(CoalRetLabels$Name))) , y = CoalRetLabels$RetTot+(CoalRetmaxxAxis/CoalRetValues[3]) , label = paste(round(CoalRetirementChartPerc$PctRetire*100,0), "% of total", start_year, "capacity",sep = " "), colour = AxisColour,size = 3)+
      theme(
        axis.title.x = element_text(colour = AxisColour, size = textsize),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour = AxisColour, size = textsize),
        axis.text.y = element_text(colour = AxisColour, size = textsize),
        axis.line.x = element_line(colour = AxisColour, size = 1),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey90", linetype = "solid"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        # panel.grid.minor.x = element_line(colour = "grey80", linetype = "dotted"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size = textsize, colour = AxisColour, margin = margin(l = 0.15, r = 0.35, unit = "cm")),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.key.size = unit(0.4, "cm"),
        legend.title = element_blank(),
        legend.key = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        plot.margin = unit(c(.4, 1.5, 0, 0), "lines")
      ) +
      coord_flip() +
      guides(guide_legend(nrow = 1))

    bar_size <- 4 / 15
    height <- (1 + maxcompanies) * bar_size + .66
  } else {
    Label <- NoCoalcompaniesinyourportfolio #
    outputplot <- no_chart(Label)
    height <- 1
  }
  ggsave(outputplot, filename = graph_name(plotnumber, ParameterFile), bg = "white", height = height, width = 10, dpi = ppi)

  sector_to_plot <- ""
}

CompareTechnologyExposure <- function(plotnumber, chart_type, sector_to_plot) {
  Combin <- data.frame()

  if (chart_type == "EQ" & has_equity == TRUE) {
    Market <- filter_by_parameters(eq_market, "EQ")
    Combin <- filter_by_parameters(EQCombin, "EQ")
  } else if (chart_type == "CB" & has_debt == TRUE) {
    Market <- filter_by_parameters(cb_market, "CB", by_equity_market = FALSE)
    Combin <- filter_by_parameters(CBCombin, "CB", by_equity_market = FALSE)
  }
  if (data_check(Combin) == TRUE) {
    PlotChart <- TRUE
  } else {
    PlotChart <- FALSE
  }

  if (PlotChart == TRUE) {


    ### Portfolio Data Structure
    Combin$Type <- "Portfolio"
    Combin <- Combin %>%
      ungroup() %>%
      filter(technology != "OilCap" & year == start_year + 5) %>% # year == PlotYr &
      select("portfolio_name", "ald_sector", "technology", "plan_alloc_wt_tech_prod", "Type") %>%
      rename(WtProduction = plan_alloc_wt_tech_prod)

    Combin <- Combin %>%
      complete(
        ald_sector = c("Oil&Gas", "Coal", "Power", "Automotive"), technology = c(technology_order),
        fill = list(WtProduction = 0, Type = "Portfolio")
      )
    Combin_A <- subset(Combin, ald_sector == "Automotive" & technology %in% c("ICE", "Electric", "Hybrid"))
    Combin_P <- subset(Combin, ald_sector == "Power" & technology %in% c("RenewablesCap", "HydroCap", "NuclearCap", "GasCap", "CoalCap"))
    Combin_F <- subset(Combin, (ald_sector == "Coal" & technology %in% c("Coal")) | (ald_sector == "Oil&Gas" & technology %in% c("Oil", "Gas")))

    Combin <- bind_rows(Combin_F, Combin_A, Combin_P)

    ### Market Data Structure

    Market$Type <- "Benchmark"
    Market <- Market %>%
      ungroup() %>%
      filter(technology != "OilCap" & year == start_year + 5) %>% # year == PlotYr & Type =="Portfolio") %>%
      select("portfolio_name", "ald_sector", "technology", "scen_alloc_wt_tech_prod", "Type") %>% # "Type"
      rename(WtProduction = scen_alloc_wt_tech_prod)

    Market <- Market %>%
      complete(
        ald_sector = c("Oil&Gas", "Coal", "Power", "Automotive"), technology = c(technology_order),
        fill = list(WtProduction = 0, Type = "Benchmark")
      )
    Market_A <- subset(Market, ald_sector == "Automotive" & technology %in% c("ICE", "Electric", "Hybrid"))
    Market_P <- subset(Market, ald_sector == "Power" & technology %in% c("RenewablesCap", "HydroCap", "NuclearCap", "GasCap", "CoalCap"))
    Market_F <- subset(Market, (ald_sector == "Coal" & technology %in% c("Coal")) | (ald_sector == "Oil&Gas" & technology %in% c("Oil", "Gas")))

    Market <- bind_rows(Market_F, Market_A, Market_P)


    ### Combine Market and Portfolios to the same Data Structure
    Production <- bind_rows(Combin, Market)


    Production$ald_sector <- factor(Production$ald_sector, levels = c("Oil&Gas", "Coal", "Power", "Automotive"))
    levels(Production$ald_sector)[levels(Production$ald_sector) == "Coal"] <- "Fossil Fuels"
    levels(Production$ald_sector)[levels(Production$ald_sector) == "Oil&Gas"] <- "Fossil Fuels"
    Production$ald_sector <- factor(Production$ald_sector, levels = c("Fossil Fuels", "Power", "Automotive"))

    # Aggregate and rename CarstenMetric_Port
    ID.COLS <- c("ald_sector", "technology", "Type")
    Production <- Production %>% gather(key = Metric, value = Value, "WtProduction")
    Production <- aggregate(Production["Value"], by = Production[c(ID.COLS)], FUN = sum)
    # Created an average for the peers (or even just use fill!)

    # Transfer coal to gigajoule for tech share charts only!!!
    Production$Value[Production$technology == "Coal"] <- Production$Value[Production$technology == "Coal"] * 29307600 / 365.25 / 1e6

    # , Production$Type != "Market"

    if (sector_to_plot != "All") {
      technologyorder <- technology_order
      colours <- as.vector(ColourPalette[["Colours"]])
      names(colours) <- technologyorder

      labels <- technologyorder
      names(labels) <- technologyorder
    } else if (sector_to_plot == "All") {
      technologyorder <- technology_order_short
      colours <- as.vector(ColourPalette[["Colours"]])[1:11]
      names(colours) <- technologyorder
      labels <- technologyorder
      names(labels) <- technologyorder
    }



    Production$technology <- factor(Production$technology, levels = technologyorder)
    Production$ald_sector <- factor(Production$ald_sector, levels = c("Fossil Fuels", "Power", "Automotive", "Aviation", "Shipping")) #

    Production$Type <- wrap_labels(Production$Type, 20)

    Production$Type <- factor(Production$Type, levels = c("Portfolio", "Benchmark"))
    xlabels <- c("Portfolio", "Aligned\nMarket")

    titles <- c("Fossil Fuel Production", "Power Capacity", "Automotive Production", "Aviation Production", "Shipping Production")
    names(titles) <- c("Fossil Fuels", "Power", "Automotive", "Aviation", "Shipping")


    Production <- subset(Production, select = c("Type", "ald_sector", "technology", "Value"))
    tot <- Production %>%
      group_by(Type, ald_sector) %>%
      summarise(Total = sum(Value))
    Production <- left_join(Production, tot, by = c("Type", "ald_sector"))
    Production$per <- Production$Value / Production$Total


    levels(Production$ald_sector) <- c("Fossil Fuels Production", "Power Capacity", "Automotive Production", "Aviation", "Shipping")
    Production$ald_sector <- dplyr::recode(Production$ald_sector,
      "Fossil Fuels Production" = wrap_labels(FossilFuelsProduction, 30), "Power Capacity" = C_Power, #
      "Automotive Production" = AutomotiveProduction, Aviation = Aviation, Shipping = Shipping
    ) #
    # Production$technology<-gsub("Cap","",Production$technology)
    Production$technology <- dplyr::recode(Production$technology,
      Gas = T_GasProd, Oil = T_OilProd, Coal = T_CoalProd, #
      RenewablesCap = paste(T_Renewables_1, "\n", T_Renewables_2), HydroCap = T_HydroCap, NuclearCap = T_NuclearCap, GasCap = T_GasCap, CoalCap = T_CoalCap, #
      Electric = x_Electric, Hybrid = x_Hybrid, ICE = x_ICE, "Freight Passenger" = FreightPassenger
    ) #

    Production$lab <- ifelse(is.nan(Production$per), "-%", paste0(round(100 * Production$per), "%"))
    Production$per <- ifelse(is.nan(Production$per), 0, Production$per)

    cmd <- ggplot(Production) +
      geom_col(aes(x = technology, y = per, fill = Type), position = "dodge", width = 0.8) +
      geom_text(aes(x = technology, y = per, label = lab, group = Type), vjust = -0.25, position = position_dodge(0.8), color = "black", size = 2) +
      scale_y_continuous(expand = c(0, 0), labels = percent, limits = c(0, 1.1)) +
      ylab(paste(technologySharein, start_year + 5)) + #
      scale_fill_manual(values = c("Portfolio" = Port, "Benchmark" = Benchmark), labels = c(Portf, AlignedMarket)) +
      theme_barcharts() +
      theme(
        plot.title = element_text(hjust = 0.5, colour = textcolor, size = 11, margin = unit(c(0, 0, 1, 0), "lines")),
        axis.text.y = element_text(size = textsize - 1.5),
        axis.text.x = element_text(size = textsize - 1.5),
        legend.position = "bottom",
        legend.title = element_blank()
      ) +
      guides(fill = guide_legend(nrow = 1)) +
      facet_grid(. ~ ald_sector, scales = "free", space = "free") +
      theme(
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = textsize)
      )


    # wid = unique(Production$technology)
  } else {
    Label <- YourPortfoliohasnoproductioninthissectorfortheselectedscenario_geographyandMarket #
    cmd <- no_chart(Label)
    # wid <- 4
  }
  ggsave(
    filename = graph_name(plotnumber, ParameterFile), # bg = "transparent",
    plot = cmd, height = 2.6, width = 7, dpi = ppi
  )
}

##############
### Themes ###
##############

theme_barcharts <- function(base_size = textsize, base_family = "") {
  theme(
    axis.ticks = element_blank(),
    axis.text.x = element_text(colour = textcolor, size = textsize),
    axis.text.y = element_text(colour = textcolor, size = textsize),
    axis.title.x = element_blank(),
    axis.title.y = element_text(colour = textcolor, size = textsize),
    axis.line.x = element_line(colour = textcolor, size = 0.5),
    axis.line.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    # legend.position=c(0.5,0),#legend.position = "none",
    legend.position = "none",
    legend.direction = "horizontal",
    legend.text = element_text(size = textsize, colour = textcolor),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key.size = unit(0.4, "cm"),
    # legend.title=element_blank(),
    legend.title = element_text(colour = textcolor, size = textsize),
    legend.key = element_blank(),
    plot.background = element_rect(fill = "transparent", colour = NA),
    plot.margin = unit(c(1, 1, 0, 0), "lines"),
    plot.title = element_blank(),
    text = element_text(family = textfont, size = textsize)
    # plot.margin = unit(c(1,1, 5, 2), "lines")
  )
}

theme_246 <- function() {
  theme_minimal(base_size = 11, base_family = textfont) +
    theme(
      panel.background = element_rect(fill = "white", color = "white"),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text = element_text(size = 11.5, color = "#3D3D3C"),
      axis.title = element_text(size = 11, color = "#3D3D3C"),
      plot.title = element_text(
        size = 12,
        hjust = 0
      )
    )
}

theme_linecharts <- function(base_size = textsize, base_family = "") {
  theme(
    axis.ticks = element_blank(),
    axis.text.x = element_text(colour = textcolor, size = textsize),
    axis.text.y = element_text(colour = textcolor, size = textsize),
    axis.title.x = element_text(colour = textcolor, size = textsize),
    axis.title.y = element_text(colour = textcolor, size = textsize),
    axis.line = element_line(colour = textcolor, size = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # panel.background = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    # legend.position=c(0.5,0),#legend.position = "none",
    legend.position = "none",
    legend.direction = "horizontal",
    legend.text = element_text(size = textsize, colour = textcolor),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key.size = unit(0.4, "cm"),
    # legend.title=element_blank(),
    legend.title = element_text(colour = textcolor, size = textsize),
    legend.key = element_blank(),
    plot.background = element_rect(fill = "transparent", colour = NA),
    plot.margin = unit(c(1, 1, 0, 0), "lines")
    # plot.margin = unit(c(1,1, 5, 2), "lines")
  )
}

theme_cdi <- function() {
  theme_minimal(base_size = 11, base_family = textfont) +
    theme(
      panel.background = element_rect(fill = "white", color = "white"),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      axis.line.x = element_line(),
      axis.line.y = element_blank(),
      axis.ticks.y = element_line(),
      plot.title = element_text(
        family = textfont,
        face = "bold",
        size = 12,
        hjust = 0
      )
    )
}

theme_distribution <- function(base_size = textsize, base_family = "") {
  theme(
    axis.ticks = element_blank(),
    axis.text.x = element_text(colour = textcolor, size = textsize),
    axis.text.y = element_text(colour = textcolor, size = textsize),
    axis.title.x = element_text(colour = textcolor, size = textsize),
    axis.title.y = element_text(colour = textcolor, size = textsize),
    # axis.line.x = element_line(colour = textcolor,size=1),
    axis.line.y = element_line(colour = textcolor, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(color = textcolor, size = textsize),
    legend.title = element_blank(),
    plot.margin = unit(c(0.6, 1.0, 2.5, 0), "lines"),
    plot.background = element_blank(),
    plot.title = element_blank(),
    text = element_text(family = textfont)
  )
}

stacked_bar_chart <- function(dat, colors, legend_labels) {
  # "item", "family", "score", "value"
  colnames <- colnames(dat)

  plottheme <- ggplot(data = dat, show.guide = TRUE) +
    geom_bar(aes_string(x = colnames[1], y = colnames[4], fill = colnames[3]),
      stat = "identity", position = "fill", width = .6
    ) +
    scale_fill_manual(values = colors, labels = legend_labels, breaks = names(legend_labels)) +
    scale_y_continuous(expand = c(0, 0), labels = percent) +
    guides(fill = guide_legend(nrow = 1)) +
    theme_barcharts()

  return(plottheme)
}

no_chart <- function(Label) {
  wrap_it <- function(x, len) {
    sapply(x, function(y) paste(strwrap(y, len), collapse = "\n"), USE.NAMES = FALSE)
  }
  wrap_labels <- function(x, len) {
    if (is.list(x)) {
      lapply(x, wrap_it, len)
    } else {
      wrap_it(x, len)
    }
  }

  outputplot <- ggplot() +
    annotate(geom = "text", x = 0, y = 0, label = wrap_labels(Label, 25), size = 5) +
    geom_blank() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_rect(fill = "transparent", colour = NA)
    )

  return(outputplot)
}
