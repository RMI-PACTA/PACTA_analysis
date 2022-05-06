library(odbc)
library(RPostgres)
library(dplyr)
library(dbplyr)
library(readr)
library(readxl)
library(keyring)
library(r2dii.utils)
library(lubridate)




# -------------------------------------------------------------------------

data_timestamp <- "2021-12-31"
quarter <- paste0(year(ymd(data_timestamp)), "Q", quarter(ymd(data_timestamp)))




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




# -------------------------------------------------------------------------

factset_db <- connect_factset_db()

# dbGetInfo(factset_db)
# dbListObjects(factset_db)
# dbListTables(factset_db)
# grep("basic", dbListTables(factset_db), value = TRUE)
# dbDisconnect(factset_db)




# company_name ------------------------------------------------------------

# sym_sec_entity::fsym_id
# sym_sec_entity::factset_entity_id
# ent_entity_coverage::factset_entity_id
# ent_entity_coverage::entity_proper_name

fsym_id__entity_id <-
  tbl(factset_db, "sym_v1_sym_sec_entity") %>%
  select(
    fsym_id,
    entity_id = factset_entity_id
  )

entity_id__entity_proper_name <-
  tbl(factset_db, "ent_v1_ent_entity_coverage") %>%
  select(
    entity_id = factset_entity_id,
    entity_proper_name
  )

fsym_id__company_name <-
  fsym_id__entity_id %>%
  left_join(entity_id__entity_proper_name, by = "entity_id") %>%
  select(
    fsym_id,
    company_name = entity_proper_name
  )




# bloomberg_id ------------------------------------------------------------

# sym_bbg::fsym_id
# sym_bbg::bbg_id

fsym_id__bloomberg_id <-
  tbl(factset_db, "sym_v1_sym_bbg") %>%
  select(
    fsym_id,
    bloomberg_id = bbg_id
  )




# country_of_domicile -----------------------------------------------------

# sym_sec_entity::fsym_id
# sym_sec_entity::factset_entity_id
# ent_entity_coverage::factset_entity_id
# ent_entity_coverage::iso_country

fsym_id__entity_id <-
  tbl(factset_db, 'sym_v1_sym_sec_entity') %>%
  select(
    fsym_id,
    entity_id = factset_entity_id
  )

entity_id__iso_country <-
  tbl(factset_db, "ent_v1_ent_entity_coverage") %>%
  select(
    entity_id = factset_entity_id,
    iso_country
  )

fsym_id__country_of_domicile <-
  fsym_id__entity_id %>%
  left_join(entity_id__iso_country, by = "entity_id") %>%
  select(
    fsym_id,
    country_of_domicile = iso_country
  )




# isin --------------------------------------------------------------------

# sym_isin::fsym_id
# sym_isin::isin

fsym_id__isin <- tbl(factset_db, 'sym_v1_sym_isin')



# bics_sector -------------------------------------------------------------

# sym_sec_entity::fsym_id
# sym_sec_entity::factset_entity_id
# ent_entity_coverage::factset_entity_id
# ent_entity_coverage::sector_code
# factset_sector_map::factset_sector_code
# factset_sector_map::factset_sector_desc

fsym_id__entity_id <-
  tbl(factset_db, "sym_v1_sym_sec_entity") %>%
  select(
    fsym_id,
    entity_id = factset_entity_id
  )

entity_id__sector_code <-
  tbl(factset_db, "ent_v1_ent_entity_coverage") %>%
  select(
    entity_id = factset_entity_id,
    sector_code
  )

sector_code__sector_desc <-
  tbl(factset_db, "ref_v2_factset_sector_map") %>%
  select(sector_code = factset_sector_code, sector_desc = factset_sector_desc)

fsym_id__bics_sector <-
  fsym_id__entity_id %>%
  left_join(entity_id__sector_code, by = "entity_id") %>%
  left_join(sector_code__sector_desc, by = "sector_code") %>%
  select(
    fsym_id,
    bics_sector = sector_desc
  )




# unit_share_price --------------------------------------------------------

# own_sec_prices
# own_sec_prices::fsym_id
# own_sec_prices::Unadj_Price
# own_sec_prices::Adj_Price

fsym_id__unit_share_price <-
  tbl(factset_db, "own_v5_own_sec_prices") %>%
  dplyr::filter(price_date == data_timestamp) %>%
  select(
    fsym_id,
    unit_share_price = adj_price
  )




# current_shares_outstanding_all_classes ----------------------------------

# own_sec_prices
# own_sec_prices::fsym_id
# own_sec_prices::Unadj_Shares_Outstanding
# own_sec_prices::Adj_Shares_Outstanding

fsym_id__current_shares_outstanding_all_classes <-
  tbl(factset_db, "own_v5_own_sec_prices") %>%
  dplyr::filter(price_date == data_timestamp) %>%
  select(
    fsym_id,
    current_shares_outstanding_all_classes = adj_shares_outstanding
  )




# asset_type --------------------------------------------------------------

# own_sec_coverage::fsym_id
# own_sec_coverage::issue_type_code
# issue_type_map::issue_type_code
# issue_type_map::asset_class_code
# asset_class_map::asset_class_code
# asset_class_map::asset_class_desc

ref_v2_issue_type_map <- tbl(factset_db, "ref_v2_issue_type_map")

ref_v2_asset_class_map <- tbl(factset_db, "ref_v2_asset_class_map")

issue_type_code__asset_class_desc <-
  ref_v2_issue_type_map %>%
  left_join(ref_v2_asset_class_map, by = "asset_class_code") %>%
  select(issue_type_code, asset_class_desc)

fsym_id__asset_type <-
  tbl(factset_db, "own_v5_own_sec_coverage") %>%
  left_join(issue_type_code__asset_class_desc, by = c("issue_type" = "issue_type_code")) %>%
  select(
    fsym_id,
    asset_type = asset_class_desc
  )




# corporate_bond_ticker ---------------------------------------------------





# security_mapped_sector --------------------------------------------------

# probably determined based on PACTA specific info




# company_id --------------------------------------------------------------

# AR's company ID - join by ISIN from AR's PAM data

pam_xlsx <- path_dropbox_2dii("PortCheck/00_Data/06_DataStore/DataStore_export_02172022/2022-02-17_AR_2021Q4_2DII-PAMS-Data.xlsx")

isin__company_id <-
  read_excel(pam_xlsx, sheet = "Company ISINs") %>%
  select(
    isin = ISIN,
    company_id = `Company ID`
  )




# -------------------------------------------------------------------------

fin_data <-
  fsym_id__isin %>%
  left_join(fsym_id__bloomberg_id, by = "fsym_id") %>%
  left_join(fsym_id__company_name, by = "fsym_id") %>%
  left_join(fsym_id__country_of_domicile, by = "fsym_id") %>%
  left_join(fsym_id__bics_sector, by = "fsym_id") %>%
  left_join(fsym_id__unit_share_price, by = "fsym_id") %>%
  left_join(fsym_id__current_shares_outstanding_all_classes, by = "fsym_id") %>%
  left_join(fsym_id__asset_type, by = "fsym_id") %>%
  # fsym_id__corporate_bond_ticker
  collect() %>%
  left_join(isin__company_id, by = "isin") %>%
  relocate(company_id)




# -------------------------------------------------------------------------

dbDisconnect(factset_db)
saveRDS(fin_data, "fin_data.rds")



