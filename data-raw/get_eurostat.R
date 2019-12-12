## Data from Eurostat database

library(dplyr)
library(forcats)
library(tidyr)
library(eurostat)

# Total national accounts
naq_eurostat <- get_eurostat("namq_10_gdp", cache = FALSE)
# national accounts 10 industies
naq10_eurostat <- get_eurostat("namq_10_a10", cache = FALSE)
# national accounts employment 10 industies
naq10e_eurostat<- get_eurostat("namq_10_a10_e", cache = FALSE)
# Unit labour cost
namq_10_lp_ulc <- get_eurostat("namq_10_lp_ulc", cache = FALSE)


# preprosessing
naq0_eurostat_dat <- naq_eurostat %>%
  # 	Current prices, million units of national currency ,  Chain linked volumes (2010), million units of national currency
  #  	Seasonally and calendar adjusted data
  # "Gross domestic product at market prices", "Exports of goods and services", "Exports of goods", "External balance of goods and services"
  filter(unit %in% c("CP_MNAC", "CP_MEUR", "CLV10_MNAC", "CLV10_MEUR"),
         s_adj %in% c("NSA", "SA","SCA"),
         na_item %in% c("B1GQ", "P6", "P61", "B11")
  ) %>%
  mutate(nace_r2 = "TOTAL")

naq10_eurostat_dat <- naq10_eurostat %>%
  # 	Current prices, million units of national currency ,  Chain linked volumes (2010), million units of national currency
  #  	Non-seasonally adjusted, Seasonally adjusted, Seasonally and calendar adjusted data
  #   "Value added, gross", "Compensation of employees", "Wages and salaries", "Employers' social contributions"
  filter(unit %in% c("CP_MNAC", "CP_MEUR", "CLV10_MNAC", "CLV10_MEUR"),
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

ulc_eurostat_dat <- namq_10_lp_ulc %>%
  filter(unit == "I15") %>%
  droplevels()


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
usethis::use_data(naq_eurostat_dat_raw, overwrite = TRUE)
usethis::use_data(ulc_eurostat_dat, overwrite = TRUE)


# Exchange rates

eur_usd_a <- get_eurostat("ert_bil_eur_a", filters = list(statinfo = "AVG", currency = "USD")) %>%
  select(time, values)

eur_usd_q<- get_eurostat("ert_bil_eur_q", filters = list(statinfo = "AVG", currency = "USD")) %>%
  select(time, values)

usethis::use_data(eur_usd_a, eur_usd_q, overwrite = TRUE)


# Effective exchange rates

ert_eff_ic_m0 <- get_eurostat("ert_eff_ic_m", cache = FALSE)
ert_eff_ic_q0 <- get_eurostat("ert_eff_ic_q", cache = FALSE)

ert_eff_ic_m <- ert_eff_ic_m0 %>%
  mutate(exch_rt_label = label_eurostat(exch_rt, dic = "exch_rt"))

ert_eff_ic_q <- ert_eff_ic_q0 %>%
  mutate(exch_rt_label = label_eurostat(exch_rt, dic = "exch_rt"))

usethis::use_data(ert_eff_ic_m, ert_eff_ic_q, overwrite = TRUE)
