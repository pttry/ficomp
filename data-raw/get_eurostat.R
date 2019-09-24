## Data from Eurostat database

library(dplyr)
library(forcats)
library(eurostat)

# Total national accounts
naq_eurostat <- get_eurostat("namq_10_gdp", cache = FALSE)
# national accounts 10 industies
naq10_eurostat <- get_eurostat("namq_10_a10", cache = FALSE)
# national accounts employment 10 industies
naq10e_eurostat<- get_eurostat("namq_10_a10_e", cache = FALSE)



# preprosessing
naq0_eurostat_dat <- naq_eurostat %>%
  # 	Current prices, million units of national currency ,  Chain linked volumes (2010), million units of national currency
  #  	Seasonally and calendar adjusted data
  # "Gross domestic product at market prices"
  filter(unit %in% c("CP_MNAC", "CLV10_MNAC"),
         s_adj %in% c("NSA", "SA","SCA"),
         na_item %in% c("B1GQ")
  ) %>%
  mutate(nace_r2 = "TOTAL")

naq10_eurostat_dat <- naq10_eurostat %>%
  # 	Current prices, million units of national currency ,  Chain linked volumes (2010), million units of national currency
  #  	Non-seasonally adjusted, Seasonally adjusted, Seasonally and calendar adjusted data
  #   "Value added, gross", "Compensation of employees", "Wages and salaries", "Employers' social contributions"
  filter(unit %in% c("CP_MNAC", "CLV10_MNAC"),
         s_adj %in% c("NSA", "SA","SCA"),
         na_item %in% c("B1G", "D1", "D11", "D12")
  )

naq10e_eurostat_dat <- naq10e_eurostat %>%
  #   Thousand hours worked, Thousand persons
  #  	Non-seasonally adjusted, Seasonally adjusted, Seasonally and calendar adjusted data
  #   Total employment domestic concept, Employees domestic concept
  filter(unit %in% c("THS_HW", "THS_PER"),
         s_adj %in% c("NSA", "SA","SCA"),
         na_item %in% c("EMP_DC", "SAL_DC")
  )



# TODO: Open / private sector
      # measurable sector: c("C", "F", "G-I", "J", "M_N")

# compine data
naq_eurostat_dat_raw <-
  naq0_eurostat_dat %>%
  bind_rows(naq10_eurostat_dat) %>%
  bind_rows(naq10e_eurostat_dat) %>%
  filter(nace_r2 %in% c("TOTAL", "C")) %>%
  mutate(unit = as_factor(unit),
         na_item = as_factor(na_item),
         geo = as_factor(geo),
         nace_r2 = as_factor(nace_r2),
         s_adj = as_factor(s_adj))

# some data is just SA. SCA is completed with SA.
naq_eurostat_dat <- naq_eurostat_dat_raw %>%
  filter(s_adj %in% c("SA","SCA")) %>%
  spread(s_adj, values) %>%
  mutate(values = coalesce(SCA, SA)) %>%
  select(-SA, -SCA)

# Save final data
usethis::use_data(naq_eurostat_dat, overwrite = TRUE)


