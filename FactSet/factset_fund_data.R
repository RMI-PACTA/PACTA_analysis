library(here)
library(fst)
library(odbc)
library(RPostgres)
library(dplyr)
library(dbplyr)
library(readr)
library(readxl)
library(keyring)

# Connection functions ----------------------------------------------------

connect_factset_db <-
  function(dbname = "charlie") {
    require(keyring)
    require(odbc)
    require(RPostgres)

    host <- "data-eval-db.postgres.database.azure.com"
    port <- 5432L

    keyring_service_name <- "2dii_factset_database"
    username <- "postgres@data-eval-db"

    if (!username %in% key_list(service = keyring_service_name)$username) {
      key_set(
        service = keyring_service_name,
        username = username,
        prompt = "Enter password for the FactSet database (it will be stored in your system's keyring): "
      )
    }
    password <- key_get(service = keyring_service_name, username = username)

    odbc::dbConnect(
      drv = RPostgres::Postgres(),
      dbname = dbname,
      host = host,
      port = port,
      user = username,
      password = password,
      options = "-c search_path=fds"
    )
  }

# Set global vars ----------------------------------------------------------

data_timestamp <- "2020-12-31"

# Load PACTA-formatted Lipper fund data -----------------------------------

fund_data <- read_fst(here("../pacta-data/2020Q4/cleaned_files/fund_data.fst"))

# Recreate fund data using FactSet ----------------------------------------

# Here I will pull a random sub-set of 1000 funds from our current Lipper fund
# data, and try to create an equivalent dataset from the FactSet data

# pull a reproducibly random sample of 1000 fund ISINs from our current data
set.seed(666)
fund_isins <- distinct(fund_data, fund_isin)
sampled_fund_isins <- sample_n(fund_isins, 1000)

# Pull the same ISINs from factset, and the corresponding factset_fund_id
factset_db <- connect_factset_db()

# symbology containing the ISIN to fsym_id link
sym_isin <- tbl(factset_db, "sym_v1_sym_isin") %>%
  collect()

# a dataset of the sampled ISINs and the corresponding fsym_id
data <- sampled_fund_isins %>%
  left_join(sym_isin, by = c(fund_isin = "isin"))

# FactSet has a dataset linking their internal fund IDs (factset_fund_id) to
# another internal identifier (fsym_id). Note there may be multiple fsym_id per
# fund.

# get the fsym_ids that are relevant to our 1000 pulled fund ISINs
sampled_fsym_ids <- unique(data$fsym_id)

# pull all `factset_fund_id`s that are relevant to our ISIN/ fsym_id list
own_ent_fund_identifiers <- tbl(factset_db, "own_v5_own_ent_fund_identifiers") %>%
  filter(fund_identifier %in% sampled_fsym_ids) %>%
  collect()

# Joining in all identifiers
data <- data %>%
  left_join(own_ent_fund_identifiers, by = c(fsym_id = "fund_identifier"))

# Sanity Check: We can see for which ISIN and fsym_id, we have a
# factset_fund_id. We can begin to get a sense of coverage here, seeing if there
# are any ISINs in the Lipper dataset, that don't have a fund_id in FactSet
data %>%
  mutate(factset_fund_id_exists = !is.na(factset_fund_id)) %>%
  group_by(factset_fund_id_exists) %>%
  summarize(isin_count = n())
# of the 1000 randomly pulled ISIN funds, 59 are missing altogether from the
# FactSet dataset (around 94% coverage, by ISIN)

sampled_factset_fund_ids <- unique(data$factset_fund_id)

# `own_fund_detail` contains the holding details of each fund, by
# `factset_fund_id`. Here, we will pull all relevant funds
own_fund_detail <- tbl(factset_db, "own_v5_own_fund_detail") %>%
  filter(factset_fund_id %in% sampled_factset_fund_ids) %>%
  filter(report_date == data_timestamp) %>%
  select(factset_fund_id, fsym_id, reported_mv) %>%
  collect()

# connect all the factset funds (where available) to details containing:
# - fsym_id associated with each holding in fund
# - reported market value associated with each holding
data <- data %>%
  select(fund_isin, factset_fund_id) %>%
  left_join(own_fund_detail)

# Sanity Check: all funds for which a factset_fund_id exists has complete
# coverage on reported_mv (complete in the sense that every security has an
# associated market value, doesn't necessarily mean each fund has comprehensive
# coverage of all securities)
data %>%
  mutate(factset_fund_id_exists = !is.na(factset_fund_id)) %>%
  filter(factset_fund_id_exists) %>%
  mutate(mv_exists = !is.na(reported_mv)) %>%
  group_by(factset_fund_id_exists) %>%
  summarize(isin_count = n())

# Using the following two issues and asset maps, we can get info on what
# asset_class is associated with each holding
ref_v2_issue_type_map <- tbl(factset_db, "ref_v2_issue_type_map")
ref_v2_asset_class_map <- tbl(factset_db, "ref_v2_asset_class_map")

issue_type_code_asset_class_desc <- ref_v2_issue_type_map %>%
  left_join(ref_v2_asset_class_map, by = "asset_class_code") %>%
  select(issue_type_code, asset_class_desc)

fsym_id_asset_type <- tbl(factset_db, "own_v5_own_sec_coverage") %>%
  left_join(
    issue_type_code_asset_class_desc,
    by = c("issue_type" = "issue_type_code")
    ) %>%
  select(fsym_id, asset_type = asset_class_desc) %>%
  collect()

# connect asset type to fund data
data <- data %>%
  left_join(fsym_id_asset_type) %>%
  select(fund_isin, reported_mv, asset_type, fsym_id)

# connect ISINs to the fsym_ids containing each
data <- data %>%
  left_join(sym_isin) %>%
  select(fund_isin, holding_isin = isin, reported_mv, asset_type)

# It's important to note that each fund may have different types of holdings:
# Equity, Fixed Income, Private Equity, Cash, Other
# We don't expect Private Equity, Cash or Other to necessarily have corresponding ISINs
# However, the Fixed Income class could be an issue.

# Let's look at coverage by asset_type
data %>%
  filter(!is.na(asset_type)) %>%
  mutate(holding_isin_exists = !is.na(holding_isin)) %>%
  group_by(fund_isin, asset_type, holding_isin_exists) %>%
  summarise(reported_mv = sum(reported_mv)) %>%
  group_by(fund_isin) %>%
  mutate(total_mv = sum(reported_mv)) %>%
  group_by(fund_isin, holding_isin_exists) %>%
  mutate(percent_coverage = reported_mv / total_mv)

# we can also look at how many funds have above 50% coverage by AuM
coverage <- data %>%
  filter(!is.na(asset_type)) %>%
  mutate(has_isin = !is.na(holding_isin)) %>%
  group_by(fund_isin, has_isin) %>%
  summarise(reported_mv = sum(reported_mv)) %>%
  group_by(fund_isin) %>%
  mutate(coverage = reported_mv / sum(reported_mv)) %>%
  filter(has_isin) %>%
  select(fund_isin, coverage)

# 617 funds by ISIN
nrow(coverage)

# 430 funds with above 50% coverage
coverage %>%
  filter(coverage > 0.5) %>%
  nrow()

# format the final FactSet data that can be a place in for fund_data
factset_fund_data <- data %>%
  group_by(fund_isin) %>%
  mutate(total_mv = sum(reported_mv)) %>%
  group_by(fund_isin, holding_isin) %>%
  summarise(isin_weight = sum(reported_mv)/sum(total_mv)) %>%
  filter(!is.na(holding_isin))

dbDisconnect(factset_db)
