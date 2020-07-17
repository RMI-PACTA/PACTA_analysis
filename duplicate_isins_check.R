market <- readRDS("/Users/jacobkastl/Dropbox (2° Investing)/PortCheck/00_Data/07_AnalysisInputs/2019Q4_06102020_2020/0_MarketPortfolios_bonds_portfolio.rda")

portfolio <- readRDS("/Users/jacobkastl/Dropbox (2° Investing)/PortCheck_v2/10_Projects/Laura_test_project_2019_jk/40_Results/Bonds_results_company.rda")

#portfolio <- portfolio %>% 
#  rename(sector = financial_sector)


test_sda <- sda_portfolio_target(market, portfolio, scenario = "B2DS", geography = "Global", ald_sector = "Aviation", start_year = 2020, target_year = 2025)






fin_data <- portfolio_fin %>% 
  filter(portfolio_name == "GV") %>% 
  group_by(isin) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n > 1) %>% 
  arrange(desc(n), isin) %>% 
  View()

portfolio_fin %>% 
  distinct(portfolio_name)
