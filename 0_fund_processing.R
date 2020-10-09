devtools::load_all()
use_r_packages()

# FIXME: This paths throw ERROR 1: ... unexpected INCOMPLETE_STRING
path <- "/Users/vincentjerosch-herold/Dropbox (2? Investing)/PortCheck/00_Data/03_FundData/Morningstar_RawData/XML_files/FundPool_2019Q4/UniverseFiles/"
index <- read_csv("/Users/vincentjerosch-herold/Dropbox (2? Investing)/PortCheck/00_Data/03_FundData/Morningstar_RawData/Administration/GlobalUniverse/2dii_fundlist.csv")
master <- read_csv("/Users/vincentjerosch-herold/Dropbox (2? Investing)/PortCheck/00_Data/03_FundData/Morningstar_RawData/Administration/GlobalUniverse/Global_FundPool_2019Q4.csv")


master_fund_list
portfolio





# first cross-reference what isins are associated with funds covered by morningstar.
project_fund_list <- portfolio %>%
  distinct(isin)
inner_join(master_fund_list, by = "isin")

# find out which funds we do not already cover and subset by id
missing_fund_list <- project_fund_list %>%
  filter(covered != TRUE) %>%
  distinct(id)

# then create a index of ids we need to load fund data for.
project_fund_list <- project_fund_list %>%
  distinct(id)

# binding to the master list of missing funds and take only distinct ids
# from here we would run the python script that pulls the missing funds and does the funds of funds looping
global_missing_fund_list <- bind_rows(missing_fund_list, global_missing_fund_list) %>%
  distinct(id)


# every fund holdings are stored as a seperated .csv so we only load what is project relevant!
# each fund will be stored with the columns: Fund.id, Holding.isin, Holding.Weight
for (i in 1:nrow(project_fund_list)) {
  id <- project_fund_list[i, "id"]

  fund <- read_csv(paste0(ANALYSIS.INPUTS.PATH, id, ".csv"))

  extracted_fund_list[[i]] <- fund
}

fund_data <- bind_rows(extracted_data_list) # binding all of the files together

# since we each fund is stored based on the id we have to match these ids to isins in the master_fund_list
# we then use isin as the primary index, which corresponds with the existing process.
fund_data <- master_fund_list %>%
  select(id, isin) %>%
  right_join(fund_data, by = "id") %>%
  select(-id)


for (i in 2:nrow(index)) {
  Fund_id <- index[i, "Fundid"]
  id <- index[i, "id"]

  xml <- read_xml(paste0(path, Fund_id, ".xml"))

  write_xml(xml, paste0(path, id, ".xml"))

  cat(paste0("#", i, "; "))
}


master <- read_csv("/Users/vincentjerosch-herold/Dropbox (2? Investing)/PortCheck/00_Data/03_FundData/Morningstar_RawData/Administration/GlobalUniverse/Global_FundPool_2019Q4.csv")

test <- master %>%
  distinct(id)

temp <- master %>%
  left_join(index, by = "id") %>%
  filter(!is.na(Fundid))
