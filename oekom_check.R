

securities <- read_csv("/Users/vincentjerosch-herold/Dropbox (2° Investing)/PortCheck/00_Data/06_DataStore/DataStore_export_08032020/2019Q4/security_financial_data.csv")

securities %>% 
  filter(isin == "XS0858089740")

old_securities <- read_csv("/Users/vincentjerosch-herold/Dropbox (2° Investing)/PortCheck/00_Data/06_DataStore/2019Q4_export_05242020/2019Q4/security_financial_data.csv")

old_securities %>% 
  filter(isin == "XS0858089740")
