## Data from Eurostat database

library(dplyr)
library(forcats)
library(eurostat)

# Total national accounts
naq_eurostat <- get_eurostat("namq_10_gdp", cache = FALSE)
# national accounts 10 industies
naq10_eurostat <- get_eurostat("namq_10_a10", cache = FALSE)



naq0_eurostat_dat <- naq_eurostat %>%
  # 	Current prices, million units of national currency ,  Chain linked volumes (2010), million units of national currency
  #  	Seasonally and calendar adjusted data
  # "Gross domestic product at market prices"
  filter(unit %in% c("CP_MNAC", "CLV10_MNAC"),
         s_adj == "SCA",
         na_item %in% c("B1GQ")
  ) %>%
  mutate(nace_r2 = "TOTAL")

naq10_eurostat_dat <- naq10_eurostat %>%
  # 	Current prices, million units of national currency ,  Chain linked volumes (2010), million units of national currency
  #  	Seasonally and calendar adjusted data
  #   "Value added, gross", "Compensation of employees", "Wages and salaries", "Employers' social contributions"
  filter(unit %in% c("CP_MNAC", "CLV10_MNAC"),
         s_adj == "SCA",
         na_item %in% c("B1G", "D1", "D11", "D12")

  )

# TODO: Open / private sector


naq_eurostat_dat <-
  naq0_eurostat_dat %>%
  bind_rows(naq10_eurostat_dat) %>%
  select(-s_adj) %>%
  filter(nace_r2 %in% c("TOTAL", "C")) %>%
  mutate(unit = as_factor(unit),
         na_item = as_factor(na_item),
         geo = as_factor(geo),
         nace_r2 = as_factor(nace_r2))

usethis::use_data(naq_eurostat_dat, overwrite = TRUE)


