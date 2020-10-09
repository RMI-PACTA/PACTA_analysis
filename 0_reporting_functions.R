set_report_parameters <- function(ParameterFilePath) {
  report_cfg <- config::get(file = ParameterFilePath)

  Contents <- as_tibble(report_cfg$Contents)
  for (x in 1:length(Contents)) {
    assign(colnames(Contents)[x], Contents[[x]], envir = .GlobalEnv)
  }


  ParameterFile <<- as_tibble(report_cfg$Naming)
  for (x in 1:length(ParameterFile)) {
    assign(colnames(ParameterFile)[x], ParameterFile[[x]], envir = .GlobalEnv)
  }

  Benchmarks <- as_tibble(report_cfg$Benchmarks)
  for (x in 1:length(Benchmarks)) {
    assign(colnames(Benchmarks)[x], Benchmarks[[x]], envir = .GlobalEnv)
  }

  start_year <<- as.numeric(Startyear)
  no_companies <<- as.numeric(No.Companies)

  if (!exists("inc_sda_approach")) {
    inc_sda_approach <<- FALSE
    print("inc_sda_approach not in parameter file. Default value set to FALSE.")
  }

  AccountingPrinciple <<- if_else(AccountingPrinciple == "PortfolioWeight", "portfolio_weight", AccountingPrinciple)
  AccountingPrinciple <<- if_else(AccountingPrinciple == "Ownership", "ownership_weight", AccountingPrinciple)
}

get_report_list <- function(Ports.Overview) {
  port.list <- Ports.Overview %>%
    select(investor_name, portfolio_name) %>%
    unique()

  port.list <- port.list %>%
    select(investor_name, portfolio_name) %>%
    group_by(investor_name) %>%
    mutate(count = n()) %>%
    unique()

  port.list$Type <- ifelse(port.list$count > 1, "Portfolio", "InvestorSingle")
  port.list$Type <- ifelse(port.list$investor_name == port.list$portfolio_name, "InvestorMeta", port.list$Type)

  return(port.list)
}

ReportData <- function() {

  # company_name <- portfolio_name_select
  #
  # company_name <- gsub("&","\\\\\\\\&",company_name)

  SizePortfolio <- subgroup_overview %>%
    filter(portfolio_name == portfolio_name_select) %>%
    distinct(portfolio_value_usd)
  SizeofPortfolio <- SizePortfolio[[1]]


  SizeofPortfolio <- comprss_long(SizeofPortfolio)
  TodaysDate <- format(Sys.Date(), format = "%d.%m.%Y")


  NoPeers <- nrow(test_list) - 1

  if (has_equity & has_debt) {
    AssetClass <- paste0("Listed equity; ", BondReference)
  } else if (has_equity & !has_debt) {
    AssetClass <- "Listed equity"
  } else if (!has_equity & has_debt) {
    AssetClass <- BondReference
  } else if (!has_equity & !has_debt) {
    AssetClass <- "No Climate Relevant Holdings"
  }

  scenarioDF <- data.frame(
    "ShortName" =
      c("CPS", "NPS", "B2DS", "BNEF", "GPER", "SDS"),
    "scenarioText" =
      c(
        "CPS: Current Policy scenario",
        "NPS: New Policy scenario",
        "B2DS: Beyond 2 Degree scenario",
        "BNEF: Bloomberg New Energy Finance",
        "GPER: Greenpeace scenario",
        "SDS: Sustainable Development scenario"
      ),
    "scenarioName" =
      c(
        "Current Policy scenario",
        "New Policy scenario",
        "Beyond 2 Degree scenario",
        "Bloomberg New Energy Finance",
        "Greenpeace scenario",
        "Sustainable Development scenario"
      ),
    "scenarioDescription" =
      c(
        "The Current Policy scenario (CPS) represents a transition if no changes were made to current policies.",
        "The New Policy scenario (NPS) represents new policies that have been put in place.",
        "The Beyond 2 Degree scenario (B2DS) is consistent with a 50\\\\% chance of limiting average future temperature rise to 1.75\\\\degree C.",
        "Bloomberg New Energy Finance scenario (BNEF) is a scenario proposed by Bloomberg.",
        "The Greenpeace scenario",
        "The Sustainable Development scenario (SDS) is consistent with a 50\\\\% chance of limiting average future temperature rise to 2\\\\degree C."
      ),
    "scenarioTemp" =
      c("6", "4", "well below 2", "2", "2", "2")
  )
  scenarioText <- as.character(scenarioDF$scenarioText[scenarioDF$ShortName %in% Scenariochoose])
  scenarioTemp <- as.character(scenarioDF$scenarioTemp[scenarioDF$ShortName %in% Scenariochoose])
  scenarioName <- as.character(scenarioDF$scenarioName[scenarioDF$ShortName %in% Scenariochoose])
  scenarioDescription <- as.character(scenarioDF$scenarioDescription[scenarioDF$ShortName %in% Scenariochoose])

  BenchmarkValue <- "market"

  if (has_sb) {
    SovBondCov <- SB.Values$SBPerc[SB.Values$investor_name == investor_name_select & SB.Values$portfolio_name == portfolio_name_select]
    SovBondCov <- ifelse(identical(SovBondCov, numeric(0)), 0, SovBondCov)
    SovBondCov <<- round(SovBondCov * 100, 1)


    sb <- read.csv(paste0(SB.DATA.PATH, "Fig.1_2 Credit ratingbycountry_Rev1_5.csv"), strip.white = T, stringsAsFactors = F)
    sb_countries <- SB.Summary %>% filter(investor_name == investor_name_select, portfolio_name == portfolio_name_select)
    downgrade_countries <- sb %>% filter(iso2c %in% sb_countries$country_of_domicile, iso2c != "CO", S.P %in% c("BBB", "BBB-"))
    sb_downgrade <- sb_countries %>%
      filter(country_of_domicile %in% downgrade_countries) %>%
      summarise(ValUSD = sum(ValueUSD, na.rm = T))
    sb_downgrade_perc <- sb_downgrade$ValUSD / sum(sb_countries$ValueUSD, na.rm = T)
    sb_downgrade_perc <- if_else(is.nan(sb_downgrade_perc), 0, sb_downgrade_perc)
    sb_downgrade_perc <<- sb_downgrade_perc * 100

    if (sb_downgrade_perc > 100) {
      print("Greater than 100% for SB percent")
    }
  }

  SectorCheck <- test_list # [test_list$portfolio_name == portfolio_name_select,]

  if (data_check(SectorCheck)) {
    HasPower <<- SectorCheck$Power.CB | SectorCheck$Power.EQ
    HasAuto <<- SectorCheck$Automotive.CB | SectorCheck$Automotive.EQ
    HasOG <<- SectorCheck$OilGas.CB | SectorCheck$OilGas.EQ
    HasCoal <<- SectorCheck$Coal.CB | SectorCheck$Coal.EQ
    HasPowerCB <<- SectorCheck$Power.CB
    HasAutoCB <<- SectorCheck$Automotive.CB
    HasOGCB <<- SectorCheck$OilGas.CB
    HasCoalCB <<- SectorCheck$Coal.CB
    HasPowerEQ <<- SectorCheck$Power.EQ
    HasAutoEQ <<- SectorCheck$Automotive.EQ
    HasOGEQ <<- SectorCheck$OilGas.EQ
    HasCoalEQ <<- SectorCheck$Coal.EQ
  }

  PeerGroupName <- gsub("([a-z])([A-Z])", "\\1 \\2", PeerGroupSelection)

  ProjectPeersRef <- PeerGroupSelection

  PeerGroupName <- ifelse(EQPeersRef == CBPeersRef & EQPeersRef == "Project", ProjectPeersRef, PeerGroupName)


  # # Check Physical Risk
  # if(HasRISK())
  # if(data_check(EQPhysicalRisk)){
  #   EQPhysicalRiskCheck <- TRUE
  # }else{EQPhysicalRiskCheck <- FALSE}
  #
  # if(data_check(CBPhysicalRisk)){
  #   CBPhysicalRiskCheck <-TRUE
  # }  else{CBPhysicalRiskCheck <- FALSE}

  # if (IsSample == T){     # Only necessary to print an unnamed report ie for sharing with interested partners.
  #   investor_name_select <- "Investor"
  #   portfolio_name_select <- "Sample Portfolio"
  # }

  # Underscores break the report hence they are removed from names
  portfolio_name_select <- gsub("_", " ", portfolio_name_select)


  ### MERGE ALL RESULTS ###
  reportdata <<- data.frame(
    c("InvestorName", investor_name_select),
    c("PortfolioName", portfolio_name_select),
    c("SizeofPortfolio", SizeofPortfolio),
    c("TodaysDate", TodaysDate),
    c("PeerGroup", PeerGroupName),
    c("scenarioText", scenarioText),
    c("scenarioName", scenarioName),
    c("scenarioValue", Scenariochoose),
    c("scenarioDescription", scenarioDescription),
    c("scenarioTemp", scenarioTemp),
    c("BenchmarkValue", BenchmarkValue),
    c("AnalysisCoverage", AnalysisCoverage),
    c("HasPower", HasPower),
    c("HasAuto", HasAuto),
    c("HasOG", HasOG),
    c("HasCoal", HasCoal),
    c("HasCarbonBudget", HasCarbonBudget),
    c("HasPowerCB", HasPowerCB),
    c("HasAutoCB", HasAutoCB),
    c("HasOGCB", HasOGCB),
    c("HasCoalCB", HasCoalCB),
    c("HasPowerEQ", HasPowerEQ),
    c("HasAutoEQ", HasAutoEQ),
    c("HasOGEQ", HasOGEQ),
    c("HasCoalEQ", HasCoalEQ),
    c("NoPeers", NoPeers),
    c("AssetClass", AssetClass) # ,
    # c("SovBondCov", SovBondCov),
    # c("sbdowngradeperc",sb_downgrade_perc)
  )

  colnames(reportdata) <- as.character(unlist(reportdata[1, ]))
  reportdata <- reportdata[-1, ]


  return(reportdata)
}

ReportGeneration <- function() {
  over <- SectorDataAnalysis()

  AnalysisCoverage <<- round(sum(over$valid_value_usd[over$financial_sector != "Not Included"]) / sum(over$valid_value_usd) * 100, 1)


  reportdata <- ReportData()
  reportdata <- reportdata %>% filter(PortfolioName == portfolio_name_select)

  # Copy in the template for the report
  text <- as.data.frame(template, stringsAsFactors = FALSE)
  colnames(text) <- "text"

  # Function removes text lines between
  # "handlenameS" and "handlenameE" ie CBPowerS, CBPowerE
  # Need to add these handles into the final doc when ready. Also determine what will be kicked out/what will remain.

  # handlename <- "AutoSector_CB"


  removetextlines <- function(handlename) {
    startpage <- which(grepl(paste0(handlename, "S"), text$text))
    endpage <- which(grepl(paste0(handlename, "E"), text$text))

    if (length(startpage) == 0 | length(endpage) == 0) {
      text <- text
    } else if (length(startpage) > 0) {
      removelist <- lapply(1:length(startpage), function(x) c(startpage[c(x)]:endpage[c(x)]))
      removelist <- melt(removelist[1:length(startpage)])
      text <- as.data.frame(text[-removelist$value, ], stringsAsFactors = FALSE)
      colnames(text) <- "text"
    } else {
      removeline <- which(grepl(handlename, text$text))
      text <- as.data.frame(text[-removeline, ], stringsAsFactors = FALSE)
      colnames(text) <- "text"
    }
    return(text)
  }

  if (!has_equity) {
    text <- removetextlines("EQSpecific")
  }

  if (!has_debt) {
    text <- removetextlines("CBSpecific")
  }



  if (!HasAuto) {
    text <- removetextlines("AutoSector_ALL")
  } else {
    if (!HasAutoCB) {
      text <- removetextlines("AutoSector_CB")
    }
    if (!HasAutoEQ) {
      text <- removetextlines("AutoSector_EQ")
    }
  }
  if (!HasPower) {
    text <- removetextlines("PowerSector_ALL")
  } else {
    if (!HasPowerCB) {
      text <- removetextlines("PowerSector_CB")
    }
    if (!HasPowerEQ) {
      text <- removetextlines("PowerSector_EQ")
    }
  }

  if (!HasOG & !HasCoal) {
    text <- removetextlines("FossilFuelSector_ALL")
  } else {
    if (!HasOGCB & !HasCoalCB) {
      text <- removetextlines("FossilFuelSector_CB")
    }
    if (!HasOGEQ & !HasCoalEQ) {
      text <- removetextlines("FossilFuelSector_EQ")
    }
    if (!HasCarbonBudget) {
      text <- removetextlines("CarbonBudget")
    }
  }

  if (!HasCoal) {
    text <- removetextlines("CoalMiningChart")
  }

  if (HasOG) {
    if (HasOGCB == F | HasOGEQ == F) {
      text <- removetextlines("FossilFuelOneSector")
    }
  }

  if (!(HasCoalCapCB | HasCoalCapEQ)) {
    text <- removetextlines("CoalCapChart")
  }

  if (!IncPeersChart) {
    text <- removetextlines("PeerComparison")
  }

  # if(!(HasRenCapCB|HasRenCapEQ)){
  #   text <- removetextlines("RenewableCapChart")
  # }

  if (!WithCompanyCharts) {
    text <- removetextlines("CompanyCharts")
  }

  if (WithCompanyCharts) {
    a <- 0
    if (has_debt) {
      a <- a + 1
    }
    if (has_equity) {
      a <- a + 1
    }

    b <- 0
    if (a == 1) {
      if (HasPower) {
        b <- b + 1
      }
      if (HasAuto) {
        b <- b + 1
      }
    } else if (a == 2) {
      text <- removetextlines("PowerAutoTwoSectors")
    }

    if (b == 2) {
      text <- removetextlines("PowerAutoOneSector")
    } else if (b == 1) {
      text <- removetextlines("PowerAutoTwoSectors")
    }
  }

  if (IncAlignment) {
    text <- removetextlines("ExposureExecSummary")
  } else {
    text <- removetextlines("AlignmentExecSummary")
  }

  if (!IncPhysicalRisk) {
    text <- removetextlines("IncludePhysicalRisk")
  }

  if (!IncOtherSectors) {
    text <- removetextlines("OtherSectors")
  }

  # text <- removetextlines("IncRanking")
  if (!IncPRI) {
    text <- removetextlines("UNPRI_Text")
  }
  if (!IncWWF) {
    text <- removetextlines("WWFSpecificInclude")
  }
  if (IncWWF) {
    text <- removetextlines("WWFSpecificExclude")
  }

  # investor_name_select <- clean_punctuation(investor_name_select)
  # portfolio_name_select <- clean_punctuation(portfolio_name_select)
  portfolio_name_select <- gsub("_", " ", portfolio_name_select)

  if (IsSample == T) { # Only necessary to print an unnamed report ie for sharing with interested partners.
    investor_name_select <- "Investor"
    portfolio_name_select <- "Sample Portfolio"
  }

  reportdata$AssetClass <- BondReference
  reportdata$ReportDate <- if (financial_timestamp == "2017Q4") {
    "31.12.2017"
  } else
  if (financial_timestamp == "2018Q4") {
    "31.12.2018"
  } else
  if (financial_timestamp == "2019Q4") {
    "31.12.2019"
  } else {
    financial_timestamp
  }

  investor_name_selectClean <- gsub("_", " ", investor_name_select)
  # investor_name_selectClean <- gsub("\("," ",investor_name_selectClean)
  # investor_name_selectClean <- gsub("/)"," ",investor_name_selectClean)
  investor_name_selectClean <- wrap_labels(investor_name_selectClean, 20)
  investor_name_selectClean <- gsub("'", "", investor_name_selectClean)

  portfolio_name_selectClean <- gsub("_", " ", portfolio_name_select)
  portfolio_name_selectClean <- wrap_labels(portfolio_name_selectClean, 20)
  portfolio_name_selectClean <- gsub("#", "", portfolio_name_selectClean)
  portfolio_name_selectClean <- gsub("'", "", portfolio_name_selectClean)


  text$text <- gsub("start_year+10", start_year + 10, text$text, fixed = T)
  text$text <- gsub("start_year+5", start_year + 5, text$text, fixed = T)
  text$text <- gsub("start_year", start_year, text$text)
  text$text <- gsub("investor_name_select", investor_name_selectClean, text$text)
  text$text <- gsub("portfolio_name_select", portfolio_name_selectClean, text$text)
  text$text <- gsub("SizeofPortfolio", paste0("\\\\$", reportdata$SizeofPortfolio), text$text)
  text$text <- gsub("TodaysDate", reportdata$TodaysDate, text$text)
  text$text <- gsub("ReportDate", reportdata$ReportDate, text$text)
  text$text <- gsub("PeerGroup", reportdata$PeerGroup, text$text)
  text$text <- gsub("AssetClass", reportdata$AssetClass, text$text)
  text$text <- gsub("AnalysisCoverage", reportdata$AnalysisCoverage, text$text)
  text$text <- gsub("scenarioText", reportdata$scenarioText, text$text)
  text$text <- gsub("scenarioName", reportdata$scenarioName, text$text)
  text$text <- gsub("scenarioValue", reportdata$scenarioValue, text$text)
  text$text <- gsub("scenarioDescription", reportdata$scenarioDescription, text$text)
  text$text <- gsub("scenarioTemp", reportdata$scenarioTemp, text$text)
  text$text <- gsub("BenchmarkValue", reportdata$BenchmarkValue, text$text)
  text$text <- gsub("EQMarketRef", eq_market_ref, text$text)
  text$text <- gsub("CBMarketRef", cb_market_ref, text$text)


  if (has_sb) {
    text$text <- gsub("SovBondCov", SovBondCov, text$text)
    text$text <- gsub("sbdowngradeperc", sb_downgrade_perc, text$text)
  }


  if (data_check(text) == FALSE) {
    NoReport <- paste0("There is no data to print in a report")
    cat(NoReport, file = "NoReport.txt", sep = "\n")
  } else {



    # Figures
    namestring <- graph_name("00", ParameterFile)
    namestring <- basename(namestring)
    namestring <- gsub("00_", "", namestring)
    FigNames <- as.data.frame(list.files(paste0(report_path), pattern = paste0(namestring)))
    colnames(FigNames) <- "Name"
    FigNames$Fig <- substring(FigNames$Name, 1, 2)

    FigureLocation <- "ReportOutputs"
    FigNames$Fig <- paste0(FigureLocation, "/Fig", FigNames$Fig)



    for (f in 1:nrow(FigNames)) {
      text$text <- gsub(FigNames$Fig[f], FigNames$Name[f], text$text, fixed = TRUE)
    }


    ReportName <- paste0("ClimateAlignmentReport_", strtrim(investor_name_select, 20), "_", strtrim(portfolio_name_select, 20))

    # if (InvestorType %in% c("InvestorSingle","InvestorMeta")){ReportName <- paste0("ClimateAlignmentReport_",investor_name_select)}

    ReportName <- gsub(" ", "", ReportName)


    if (nchar(ReportName) > 65) {
      ReportName <- strtrim(ReportName, 65)
    }

    if (WithCompanyCharts == F) {
      ReportName <- paste0(ReportName, "_WO_CC")
    }


    # Save the template file
    write_utf8_rnw(text, paste0(report_path, ReportName, ".Rnw"))

    # write.table(text,paste0(report_path,ReportName,".Rnw"),col.names = FALSE,row.names = FALSE,quote=FALSE)

    # Copy in Report Graphics
    originalloc <- paste0(getwd(), "/Templates/ReportGraphics/")
    graphicsloc <- paste0(report_path, "ReportGraphics/")
    flist <- list.files(originalloc, full.names = TRUE)

    if (!dir.exists(file.path(graphicsloc))) {
      dir.create(file.path(graphicsloc), showWarnings = TRUE, recursive = FALSE, mode = "0777")
      for (file in flist) {
        file.copy(file, graphicsloc)
      }
    }

    owd <- getwd()
    setwd(report_path)
    # Create the PDF
    knit2pdf(paste0(report_path, ReportName, ".Rnw"), compiler = "xelatex")

    # Delete remaining files and ReportGraphics Folder
    excessfileendings <- c(".Rnw", ".tex")
    file.remove(paste0(ReportName, excessfileendings))
    unlink(paste0(report_path, "ReportGraphics"), recursive = TRUE)

    setwd(owd)
  }
  return()
}

read_utf8_tex <-
  function(file) {
    opts <- options(encoding = "native.enc")
    on.exit(options(opts), add = TRUE)

    readLines(file, encoding = "UTF-8")
  }

write_utf8_rnw <-
  function(text, file) {
    opts <- options(encoding = "native.enc")
    on.exit(options(opts), add = TRUE)

    con <- file(file, encoding = "native.enc")
    writeLines(enc2utf8(text$text), con = con, useBytes = TRUE, sep = "\n")
    close(con)
  }


##############
### Charts ###
##############

SectorDataAnalysis <- function() {
  over <- subgroup_overview
  over <- subset(over, over$portfolio_name == portfolio_name_select & valid_input == 1)

  over$financial_sector <- ifelse(over$financial_sector %in% c("Coal", "Oil&Gas"), "Fossil Fuels", over$financial_sector)
  over$financial_sector <- ifelse(over$financial_sector %in% c("Aviation", "Shipping"), "Aviation & Shipping", over$financial_sector)
  over$financial_sector <- ifelse(over$financial_sector %in% c("Steel", "Cement", "Cement & Steel"), "Cement & Steel", over$financial_sector)
  over$financial_sector <- ifelse(over$financial_sector %in% c("Other"), "Not Included", over$financial_sector)


  return(over)
}


PercentageOfPortfolioAssessed <- function(plotnumber) {
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

  ggsave(plot, filename = graph_name(plotnumber, ParameterFile), bg = "white", height = 2.3, width = 4.5, dpi = ppi) # linewidth_in*.9
}

ScopeOfAnalysis <- function(plotnumber) {
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


  ggsave(plot, filename = graph_name(plotnumber, ParameterFile), bg = "white", height = 2.2, width = 4.5, dpi = ppi) # linewidth_in*.9
}
