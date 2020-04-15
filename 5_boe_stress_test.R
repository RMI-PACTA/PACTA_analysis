# 5_stress_test

library(tidyr)
library(dplyr)
library(scales)
library(reshape2)
library(scales)
library("RColorBrewer")
library(ggplot2)
library(lme4)


# get data

revenue <- readRDS(paste0("ReferenceData/",FinDataTimeStamp,"/TotalRevenueList_BoE.rda"))

Average.Sector.Weights <- read.csv(paste0("ReferenceData/IndustryTechShares.csv"), strip.white = T, stringsAsFactors = F)
Averaged.Sectors <- unique(Average.Sector.Weights$BoE.Sector)

# set Sectors

Sectors = c(rep("Fuel Extraction",3), rep("Power",4),rep("Automotive",2),"Shipping","Aviation",rep("Materials",2),"Agriculture","Food Logistics","Real Estate","Water","All other sectors")
SubSectors = c(c("Coal","Oil","Gas"),c("Coal Power","Oil Power","Gas Power"),"Low Carbon","Non Electric","Electric", rep("-",2), "Fossil Fuel Based","Other", rep("-",5))

Sector.List <- data.frame("Sector" = Sectors, "Subsector" = SubSectors)



End.Year <- 2040
Port.Years <- seq(Startyear,Startyear+5,1)

all_holdings <- read_csv(paste0(proc_input_path,"/",project_name,"_audit_file.csv"))



