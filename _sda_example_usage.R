library(tibble)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

source("0_sda_approach.R")

b2ds_scen_emission_factors <-
  tibble::tribble(
    ~scenario, ~ald_sector, ~year, ~value,
    "B2DS",    "Cement",    2014,  0.582644415615628,
    "B2DS",    "Cement",    2025,  0.501438531033769,
    "B2DS",    "Cement",    2030,  0.430393853942256,
    "B2DS",    "Cement",    2035,  0.360690452957794,
    "B2DS",    "Cement",    2040,  0.282732742468256,
    "B2DS",    "Cement",    2045,  0.242959859089995,
    "B2DS",    "Cement",    2050,  0.168966172912532,
    "B2DS",    "Steel",     2014,  1.80759846130986,
    "B2DS",    "Steel",     2025,  1.1287448939987,
    "B2DS",    "Steel",     2030,  0.874539236865355,
    "B2DS",    "Steel",     2035,  0.63380312584879,
    "B2DS",    "Steel",     2040,  0.496436820917842,
    "B2DS",    "Steel",     2045,  0.380201231895074,
    "B2DS",    "Steel",     2050,  0.260550997168052
  )

sds_scen_emission_factors <-
  tibble::tribble(
    ~scenario, ~ald_sector, ~year, ~value,
    "SDS",     "Cement",    2014,  0.582644415615628,
    "SDS",     "Cement",    2025,  0.568190389759802,
    "SDS",     "Cement",    2030,  0.517417513262234,
    "SDS",     "Cement",    2035,  0.482333490096381,
    "SDS",     "Cement",    2040,  0.428700683412487,
    "SDS",     "Cement",    2045,  0.380573795422979,
    "SDS",     "Cement",    2050,  0.335885909445562,
    "SDS",     "Steel",     2014,  1.80759846130986,
    "SDS",     "Steel",     2025,  1.4602673281363,
    "SDS",     "Steel",     2030,  1.19122846594663,
    "SDS",     "Steel",     2035,  0.991959987602403,
    "SDS",     "Steel",     2040,  0.838046921242816,
    "SDS",     "Steel",     2045,  0.691548478997344,
    "SDS",     "Steel",     2050,  0.630416010220265
  )

scen_emission_factors <- bind_rows(b2ds_scen_emission_factors, sds_scen_emission_factors)


portfolio <-
  readRDS("Equity_results_portfolio.rda") %>%
  filter(investor_name == first(investor_name)) %>%
  filter(portfolio_name == first(portfolio_name)) %>%
  filter(scenario_geography == "Global") %>%
  filter(allocation == "portfolio_weight") %>%
  as_tibble()

# fake SDS data for Cement and Steel
portfolio <-
  portfolio %>%
  filter(scenario == "B2DS", ald_sector %in% c("Cement", "Steel")) %>%
  mutate(scenario = "SDS") %>%
  bind_rows(portfolio)


portfolio %>%
  mutate(sda_target = calc_sda_target(., scen_emission_fctrs = scen_emission_factors)) %>%
  filter(grepl("B2DS|SDS", scenario, ignore.case = TRUE)) %>%
  select(scenario, ald_sector, year, plan_sec_emissions_factor,
         scen_sec_emissions_factor, sda_target) %>%
  distinct() %>%
  arrange(scenario, ald_sector, year) %>%
  pivot_longer(cols = !c(scenario, ald_sector, year)) %>%
  unite("name", scenario, name) %>%
  mutate(name = if_else(grepl("_plan_sec_emissions_factor$", name), "plan_sec_emissions_factor", name)) %>%
  distinct() %>%
  filter(!is.na(value)) %>%
  mutate(year = lubridate::make_date(year)) %>%
  ggplot() +
  geom_line(aes(x = year, y = value, color = name), size = 0.50) +
  facet_wrap(~ald_sector, scales = "free") +
  labs(
    x = "Year",
    y = "Emission Factor",
    color = "Type of indicator"
  ) +
  theme(text = element_text(size = 10))


# devtools::install_github("2DegreesInvesting/r2dii.plot.static")
library(r2dii.plot.static)

cement_or_steel <- "Cement"

portfolio %>%
  mutate(sda_target = calc_sda_target(., scen_emission_fctrs = scen_emission_factors)) %>%
  filter(grepl("B2DS|SDS", scenario, ignore.case = TRUE)) %>%
  filter(ald_sector == cement_or_steel) %>%
  filter(scenario_geography == "Global") %>%
  filter(allocation == "portfolio_weight") %>%
  select(scenario, ald_sector, year, plan_sec_emissions_factor,
         scen_sec_emissions_factor, sda_target) %>%
  distinct() %>%
  arrange(ald_sector, year) %>%
  pivot_longer(cols = !c(scenario, ald_sector, year)) %>%
  unite("name", scenario, name) %>%
  mutate(name = if_else(grepl("_plan_sec_emissions_factor$", name), "plan_sec_emissions_factor", name)) %>%
  distinct() %>%
  filter(!is.na(value)) %>%
  rename(sector = ald_sector) %>%
  mutate(sector = case_when(
    sector == "Cement" ~ "cement",
    sector == "Steel" ~ "steel",
    TRUE ~ sector
  )) %>%
  prepare_for_timeline(sector_filter = tolower(cement_or_steel), column_line_names = "name", value_to_plot = "value") %>%
  plot_timeline()
