check_fin <- portfolio_fin %>% select(isin) %>% group_by(isin) %>% summarise(n = n()) %>% ungroup() %>% arrange(desc(n))

check_isin <- portfolio_isin %>% select(isin) %>% group_by(isin) %>% summarise(n = n()) %>% ungroup() %>% arrange(desc(n))

check <- check_fin %>% 
  left_join(check_isin, by = c("isin")) %>% 
  mutate(diff = n.x - n.y) %>% 
  filter(diff > 0) %>% 
  arrange(desc(diff))

check_isin <- check %>% pull(isin)

portfolio_isin %>% filter(isin %in% check_isin) %>% arrange(isin) %>% nrow()

portfolio_fin %>% filter(isin %in% check_isin) %>% arrange(isin) %>% View()

portfolio %>% filter(isin %in% check_isin) %>% arrange(isin) %>% View()

fin_data_conflicts <- fin_data %>% filter(isin %in% check_isin) %>% arrange(isin)

write_csv(fin_data_conflicts, "/Users/jacobkastl/Desktop/fin_data_conflicts.csv")

fin_data_raw %>% filter(isin %in% check_isin) %>% arrange(isin) %>% View()
fin_data_raw %>% distinct(figi) %>% nrow()

figi_remove <- c("BBG006SCSYG7", #domicile is CA, this figi refers to US
  "BBG00709J003", #could not find the corresponding ticker on bbg website
  "BBG00FGWRBB2", "BBG00MZ1P7Q9", "BBG00FX68RK6", "BBG0043GLPF7", "BBG00K7KTK07", "BBG00FX69H54")

fin_data_raw %>% filter(!(figi %in% figi_remove)) %>% nrow()

fin_data_raw <- fin_data_raw %>% filter(isin %in% check_isin) %>% 
  filter(figi != "BBG006SCSYG7" & #domicile is CA, this figi refers to US
           figi != "BBG00709J003" & #could not find the corresponding ticker on bbg website
           figi != "BBG00FGWRBB2" &
           figi != "BBG00MZ1P7Q9" &
           figi != "BBG00FX68RK6" &
           figi != "BBG0043GLPF7" &
           figi != "BBG00K7KTK07" &
           figi != "BBG00FX69H54")

non_distinct_isins <- read_csv("/Users/jacobkastl/Desktop/non_distinct_isins.csv")

non_distinct_isins_conflicts <- non_distinct_isins %>% filter(isin %in% check_isin) %>% arrange(isin)

write_csv(non_distinct_isins_conflicts, "/Users/jacobkastl/Desktop/non_distinct_isins_conflicts.csv")
