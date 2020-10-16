# new_data prep



currencies <- get_and_clean_currency_data()

# fund_data <- get_and_clean_fund_data()
fund_data <- data.frame()

fin_data <- get_and_clean_fin_data(fund_data)

comp_fin_data <- get_and_clean_company_fin_data()


colnames(comp_fin_data)



debt_fin_data <- get_and_clean_debt_fin_data()

# revenue_data <- get_and_clean_revenue_data()

average_sector_intensity <- get_average_emission_data(inc_emission_factors)

company_emissions <- get_company_emission_data(inc_emission_factors)
company_emissions <- company_emissions %>% rename(financial_sector = mapped_sector)


save_cleaned_files(
  file_location,
  currencies,
  fund_data,
  fin_data,
  comp_fin_data,
  debt_fin_data,
  average_sector_intensity,
  company_emissions
)
