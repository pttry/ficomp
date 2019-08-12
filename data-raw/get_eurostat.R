## Data from Eurostat database

library(dplyr)
library(eurostat)

# Total national accounts
naq_eurostat <- get_eurostat("namq_10_gdp", cache = FALSE)
# national accounts 10 industies
naq10_eurostat <- get_eurostat("namq_10_a10", cache = FALSE)



naq0_eurostat_dat <- naq_eurostat %>%
  # 	Current prices, million units of national currency ,  Chain linked volumes (2010), million units of national currency
  #  	Seasonally and calendar adjusted data
  # "Gross domestic product at market prices", "Value added, gross", "Compensation of employees", "Wages and salaries", "Employers' social contributions"
  filter(unit %in% c("CP_MNAC", "CLV10_MNAC"),
         s_adj == "SCA",
         na_item %in% c("B1GQ")
  ) %>%
  mutate(nace_r2 = "TOTAL")

naq10_eurostat_dat <- naq10_eurostat %>%
  # 	Current prices, million units of national currency ,  Chain linked volumes (2010), million units of national currency
  #  	Seasonally and calendar adjusted data
  # "Gross domestic product at market prices", "Value added, gross", "Compensation of employees", "Wages and salaries", "Employers' social contributions"
  filter(unit %in% c("CP_MNAC", "CLV10_MNAC"),
         s_adj == "SCA",
         na_item %in% c("B1G", "D1", "D11", "D12")
  )

naq_eurostat_dat <-
  naq0_eurostat_dat %>%
  bind_rows(naq10_eurostat_dat) %>%
  select(-s_adj)

usethis::use_data(naq_eurostat_dat, overwrite = TRUE)



naq10_eurostat_dat %>%
  filter(geo == "FI",
         time == as.Date("2019-01-01"),
         nace_r2 == "TOTAL")
