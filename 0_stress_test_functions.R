# boe_stress_test_functions

# 6_StressTest

rm(list=ls())

options(encoding = "Windows-1252")

library(tidyr)
library(dplyr)
library(scales)
library(reshape2)
# library(tidyverse)
# library(readxl)
library(scales)
library("RColorBrewer")
library(ggplot2)
library(lme4)

################################
# I) Get location of the code ----
################################
# code and portfolio data should be in the same folder in this case
if (rstudioapi::isAvailable()) {
  Location <- dirname(rstudioapi::getActiveDocumentContext()$path)
  Location <- gsub("?","",Location)
} else {
  Location <- getwd()
}

Location <- gsub("?","",Location)
Location <- paste0(Location, "/")


################################
# II) Set workdrive ----
################################
setwd(Location)

source("0_GlobalPortCheckFunctions.R", encoding = "WINDOWS-1252")
source("0_PortfolioInputCheck2.R", encoding = "WINDOWS-1252")
source("0_WebGraphFunctions.R", encoding = "WINDOWS-1252")

if (rstudioapi::isAvailable()) {
  PortfolioNameRef <- "Portfolio3"
} else {
  PortfolioNameRef = GetPortfolioName()
}



### Load in Audit File ###


process_audit_filt <- function(all_holdings){
  port <- merge(all_holdings, revenue, by = "isin", all.x = T)
  
  port$Value.USD <- as.numeric(port$Value.USD)
  
  initial.value = sum(port$Value.USD)
  
  
}



Port$Rev.Value <- Port$Value.USD * Port$Tot.Rev/100
Port$Port.Wt <- Port$Rev.Value / initial.value

Port$Port.Wt[is.na(Port$Port.Wt)] <- Port$Value.USD[is.na(Port$Port.Wt)]/initial.value
Port$BoE.Sector[is.na(Port$BoE.Sector)] <- "Other"
Port$BoE.Sub.Sector[is.na(Port$BoE.Sub.Sector)] <- "-"
Port$BoE.Sub.Sector[Port$BoE.Sector == "Other"] <- "-"

Port.EQ <- Port[Port$Asset.Type == "Equity",]
Port.CB <- Port[Port$Asset.Type == "Bonds",]


BoE.ISINs <- Port %>% 
  select(Holding.ID, ISIN,Company.Name, BoE.Sector, BoE.Sub.Sector, Value.USD, Tot.Rev, Rev.Value) %>% 
  group_by(Holding.ID, ISIN) %>% 
  mutate(count.check = n(),
         Market.Value.USD = Value.USD,
         Revenue.Split.Percentage = if_else(is.na(Tot.Rev) & count.check == 1,100, Tot.Rev),
         Revenue.Total.USD = Revenue.Split.Percentage * Value.USD/100) %>%
  arrange(Holding.ID) %>% 
  select(-count.check, -Tot.Rev, -Rev.Value, -Value.USD)

Calculate.Portfolio.Revenues <- function(Port){
  
  Sector.Split <- Port %>% group_by(BoE.Sector, BoE.Sub.Sector) %>% summarise(Sector.Weight = sum(Port.Wt, na.rm = T))
  
  # Add in the averaged technology splits
  Sector.Split.Missing <- Sector.Split %>% 
    filter(BoE.Sector %in% Averaged.Sectors & BoE.Sub.Sector == "-") %>% 
    select(-BoE.Sub.Sector)
  Sector.Split.Missing <- merge(Sector.Split.Missing, Average.Sector.Weights, by = "BoE.Sector", all = T)
  Sector.Split.Missing <- Sector.Split.Missing %>% 
    mutate(Average.Sector.Weight = AverageTechShare * Sector.Weight) %>% 
    select(-AverageTechShare, -Sector.Weight)
  
  Sector.Split <- Sector.Split %>% 
    filter(!BoE.Sector %in% Averaged.Sectors|BoE.Sector %in% Averaged.Sectors & BoE.Sub.Sector != "-")
  
  
  Sector.Split.Complete <- merge(Sector.Split, Sector.Split.Missing, by = c("BoE.Sector", "BoE.Sub.Sector"), all = T)
  
  Sector.Split.Complete <- Sector.Split.Complete %>% 
    rowwise() %>%
    mutate(Sector.Weight = sum(Average.Sector.Weight,Sector.Weight,na.rm = T),
           BoE.Sector = as.character(BoE.Sector)) %>%
    select(-Average.Sector.Weight) 
  Sector.Split.Complete$BoE.Sector <- if_else(Sector.Split.Complete$BoE.Sector == "Other", "All other sectors", Sector.Split.Complete$BoE.Sector)
  
  Results <- merge(Sector.List,Sector.Split.Complete, by.x = c("Sector","Subsector"), by.y = c("BoE.Sector", "BoE.Sub.Sector"), all = T)
  
  Results$Sector.Weight <- ifelse(is.na(Results$Sector.Weight),0,Results$Sector.Weight)
  
  Results
  
}

Revs.EQ <- Sector.List %>% mutate(Equity.Exposure = 0)
Revs.CB <- Sector.List %>% mutate(Bond.Exposure = 0)

if(DataCheck(Port.EQ)){
  Revs.EQ <- Calculate.Portfolio.Revenues(Port.EQ) %>% rename(Equity.Exposure = Sector.Weight)
}
if(DataCheck(Port.CB)){
  Revs.CB <- Calculate.Portfolio.Revenues(Port.CB) %>% rename(Bond.Exposure = Sector.Weight)
}

Results <- merge(Revs.EQ, Revs.CB, by = c("Sector","Subsector"), all = T)

Results$Label <- paste0(Results$Sector,":",Results$Subsector)
Sector.List$Label <- paste0(Sector.List$Sector,":",Sector.List$Subsector)
Sector.List.Con <- (Sector.List$Label)
Results <- Results %>% complete(Label = Sector.List.Con)

Results$Sector <- gsub(":.*","",Results$Label)
Results$Subsector <- gsub(".*:","",Results$Label)
Results <- Results %>% select(-Label)

Results$Equity.Exposure[is.na(Results$Equity.Exposure)] <- 0
Results$Bond.Exposure[is.na(Results$Bond.Exposure)] <- 0

# Sort the order for the excel tool
Results$Sector <- factor(Results$Sector, levels = unique(Sectors), ordered = T)
Results$Subsector <- factor(Results$Subsector, levels = unique(SubSectors), ordered = T)
Results <- Results %>% arrange(Sector, Subsector)

Results <- Results[!is.na(Results$Sector),]


EQTot <- 1 - sum(Results$Equity.Exposure, na.rm = T)
CBTot <- 1 - sum(Results$Bond.Exposure, na.rm = T)

Results$Equity.Exposure[Results$Sector == "All other sectors"] <- min(1, Results$Equity.Exposure[Results$Sector == "All other sectors"] + EQTot)
Results$Bond.Exposure[Results$Sector == "All other sectors"] <- min(1, Results$Bond.Exposure[Results$Sector == "All other sectors"] + CBTot)


write.csv(Results, paste0(Location,Port.Results.Path,"/StressTestInputs_BoE.csv"), row.names = F)
write.csv(BoE.ISINs, paste0(Location, Port.Results.Path, "/StressTestInputs_ISINs.csv"), row.names = F)

