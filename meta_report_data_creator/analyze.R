data %>%
  group_by(Investor.Name) %>%
  summarise(
    portfolios = length(unique(Portfolio.Name)),
    holdings = n(),
    value = sum(MarketValue, na.rm = TRUE)
    ) %>%
  arrange(desc(value)) %>%
  print(n = 50)
