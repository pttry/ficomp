## Data from Eurostat database

library(dplyr)
library(forcats)
library(tidyr)
library(eurostat)

devtools::load_all()





### Quaterly national accounts

## Get tables
# Total national accounts
naq_eurostat <- get_eurostat("namq_10_gdp", cache = FALSE)
# national accounts 10 industies
naq10_eurostat <- get_eurostat("namq_10_a10", cache = FALSE)
# national accounts employment 10 industies
naq10e_eurostat<- get_eurostat("namq_10_a10_e", cache = FALSE)
# Unit labour cost
namq_10_lp_ulc <- get_eurostat("namq_10_lp_ulc", cache = FALSE)
# Labour cost index
lc_lci_r2_q <- get_eurostat("lc_lci_r2_q", cache = FALSE)


## Preprosessing tables
naq0_eurostat_dat <- naq_eurostat %>%
  # 	Current prices, million units of national currency ,  Chain linked volumes (2010), million units of national currency
  #  	Seasonally and calendar adjusted data
  # "Gross domestic product at market prices", "Exports of goods and services", "Exports of goods", "Exports of services", "External balance of goods and services"
  filter(unit %in% c("CP_MNAC", "CP_MEUR", "CLV15_MNAC"),
         s_adj %in% c("NSA", "SA","SCA"),
         na_item %in% c("B1GQ", "P6", "P61", "P62", "P7", "B11")
  ) %>%
  mutate(nace_r2 = "TOTAL") %>%
  droplevels()

naq10_eurostat_dat <- naq10_eurostat %>%
  # 	Current prices, million units of national currency ,  Chain linked volumes (2010), million units of national currency
  #  	Non-seasonally adjusted, Seasonally adjusted, Seasonally and calendar adjusted data
  #   "Value added, gross", "Compensation of employees", "Wages and salaries", "Employers' social contributions"
  filter(unit %in% c("CP_MNAC", "CP_MEUR", "CLV15_MNAC"),
         s_adj %in% c("NSA", "SA","SCA"),
         na_item %in% c("B1G", "D1", "D11", "D12")
  ) %>%
  droplevels()

naq10e_eurostat_dat <- naq10e_eurostat %>%
  #   Thousand hours worked, Thousand persons
  #  	Non-seasonally adjusted, Seasonally adjusted, Seasonally and calendar adjusted data
  #   Total employment domestic concept, Employees domestic concept
  filter(unit %in% c("THS_HW", "THS_PER"),
         s_adj %in% c("NSA", "SA","SCA"),
         na_item %in% c("EMP_DC", "SAL_DC")
  ) %>%
  droplevels()

ulc_eurostat_dat <- namq_10_lp_ulc %>%
  filter(unit == "I15") %>%
  droplevels()

lci_eurostat_dat <- lc_lci_r2_q %>%
  #   Index, 2016=100
  #  	 Seasonally and calendar adjusted data
  #   Industry, construction and services (except activities of households as employers and extra-territorial organisations and bodies)
  #   Labour cost for LCI (compensation of employees plus taxes minus subsidies)
  filter(unit %in% c("I16"),
         s_adj %in% c("SCA", "SA"),
         nace_r2 %in% c("B-S"),
         lcstruct %in% c("D1_D4_MD5")
  ) %>%
  mutate(nace_r2 = "TOTAL") %>%
  rename(na_item = lcstruct) %>%
  droplevels()


## compine data
naq_eurostat_dat_raw <-
  naq0_eurostat_dat %>%
  bind_rows(naq10_eurostat_dat) %>%
  bind_rows(naq10e_eurostat_dat) %>%
  bind_rows(lci_eurostat_dat) %>%
  mutate(unit = as_factor(unit),
         na_item = as_factor(na_item),
         geo = as_factor(geo),
         nace_r2 = as_factor(nace_r2),
         s_adj = as_factor(s_adj))

# some data is just SA. SCA is completed with SA.
naq_eurostat_dat <- naq_eurostat_dat_raw %>%
  filter(nace_r2 %in% c("TOTAL", "C")) %>%
  filter(s_adj %in% c("SA","SCA")) %>%
  spread(s_adj, values) %>%
  mutate(values = coalesce(SCA, SA)) %>%
  select(-SA, -SCA) %>%
  droplevels()

naq_eurostat_nace_dat <- naq_eurostat_dat_raw %>%
  filter(s_adj %in% c("SA","SCA")) %>%
  filter(nace_r2 %in% main_nace_sna_q,
         geo %in% eurostat_geos,
         time >= q_start_time) %>%
  # remove na_item without nace data
  filter(!(na_item %in% c("B1GQ", "P6", "P61", "P62", "P7", "B11"))) %>%
  spread(s_adj, values) %>%
  mutate(values = coalesce(SCA, SA)) %>%
  select(-SA, -SCA) %>%
  droplevels()

# naq_eurostat_nace_dat_long <- naq_eurostat_dat_raw %>%
#   filter(s_adj %in% c("SA","SCA")) %>%
#   filter(nace_r2 %in% main_nace_sna_q,
#          geo %in% eurostat_geos) %>%
#   # remove na_item without nace data
#   filter(!(na_item %in% c("B1GQ", "P6", "P61", "P62", "B11"))) %>%
#   spread(s_adj, values) %>%
#   mutate(values = coalesce(SCA, SA)) %>%
#   select(-SA, -SCA) %>%
#   droplevels()


# Save final data
usethis::use_data(naq_eurostat_dat, overwrite = TRUE)
usethis::use_data(naq_eurostat_nace_dat, overwrite = TRUE)
usethis::use_data(naq_eurostat_dat_raw, overwrite = TRUE)
usethis::use_data(ulc_eurostat_dat, overwrite = TRUE)


### Exchange rates


currencies <- c(AU = "AUD", CA = "CAD", US = "USD", JP = "JPY", NZ = "NZD", CH = "CHF")

exh_eur_a <- get_eurostat("ert_bil_eur_a", filters = list(statinfo = "AVG", currency = currencies), time_format = "num") %>%
  select(time, currency, values) %>%
  mutate(geo = recode(currency, !!!purrr::set_names(names(currencies), currencies)))

exh_eur_q <- get_eurostat("ert_bil_eur_q", filters = list(statinfo = "AVG", currency = currencies)) %>%
  select(time, currency, values) %>%
  mutate(geo = recode(currency, !!!purrr::set_names(names(currencies), currencies)))

usethis::use_data(exh_eur_a, exh_eur_q, overwrite = TRUE)


### Effective exchange rates

ert_eff_ic_m0 <- get_eurostat("ert_eff_ic_m", cache = FALSE)
ert_eff_ic_q0 <- get_eurostat("ert_eff_ic_q", cache = FALSE)

ert_eff_ic_m <- ert_eff_ic_m0 %>%
  mutate(exch_rt_label = label_eurostat(exch_rt, dic = "exch_rt")) %>%
  mutate_if(is.character, as_factor)


ert_eff_ic_q <- ert_eff_ic_q0 %>%
  mutate(exch_rt_label = label_eurostat(exch_rt, dic = "exch_rt"))%>%
  mutate_if(is.character, as_factor)

usethis::use_data(ert_eff_ic_m, ert_eff_ic_q, overwrite = TRUE)


# Annual national accounts


dat_nama_10_gdp_0 <- eurostat::get_eurostat("nama_10_gdp", time_format = "num", cache = FALSE)
dat_nama_10_a10_0 <- eurostat::get_eurostat("nama_10_a10", time_format = "num", cache = FALSE)
dat_nama_10_a10_e_0 <- eurostat::get_eurostat("nama_10_a10_e", time_format = "num", cache = FALSE)
dat_nama_10_a64_0 <- eurostat::get_eurostat("nama_10_a64", time_format = "num", cache = FALSE)
dat_nama_10_a64_e_0 <- eurostat::get_eurostat("nama_10_a64_e", time_format = "num", cache = FALSE)

dat_nama_10_gdp <- dat_nama_10_gdp_0 %>%
  filter(unit %in% c("CLV15_MNAC", "CP_MNAC", "CP_MEUR"),
         na_item %in% c("B1GQ", "P6", "P61", "P62", "P7", "B11")) %>%
  unite(vars, na_item, unit, sep = "__") %>%
  mutate(vars = as_factor(vars),
         nace_r2 = as_factor("TOTAL")) %>%
  spread(vars, values)

dat_nama_10_a10 <- dat_nama_10_a10_0 %>%
  filter(unit %in% c("CLV15_MNAC", "CP_MNAC", "CP_MEUR", "PYP_MNAC"),
         na_item %in% c("B1G", "D1")) %>%
  unite(vars, na_item, unit, sep = "__") %>%
  mutate(vars = as_factor(vars)) %>%
  spread(vars, values)

dat_nama_10_a10_e <- dat_nama_10_a10_e_0 %>%
  filter(unit %in% c("THS_HW", "THS_PER"),
         na_item %in% c("EMP_DC", "SAL_DC")) %>%
  unite(vars, na_item, unit, sep = "__") %>%
  mutate(vars = as_factor(vars)) %>%
  spread(vars, values)

dat_nama_10_a64 <- dat_nama_10_a64_0 %>%
  filter(unit %in% c("CLV15_MNAC", "CP_MNAC", "PYP_MNAC", "CP_MEUR"),
         na_item %in% c("B1G", "D1")) %>%
  unite(vars, na_item, unit, sep = "__") %>%
  mutate(vars = as_factor(vars)) %>%
  spread(vars, values) %>%
  select(- D1__CLV15_MNAC, -D1__PYP_MNAC)

dat_nama_10_a64_e <- dat_nama_10_a64_e_0 %>%
  filter(unit %in% c("THS_HW", "THS_PER"),
         na_item %in% c("EMP_DC", "SAL_DC")) %>%
  unite(vars, na_item, unit, sep = "__") %>%
  mutate(vars = as_factor(vars)) %>%
  spread(vars, values)


# Main nace 64
dat_eurostat_nace <-
  dat_nama_10_a64 %>%
  left_join(dat_nama_10_a64_e, by = c("nace_r2", "geo", "time")) %>%
  mutate_at(c("geo", "nace_r2"), as_factor) %>%
  filter(nace_r2 %in% main_nace_sna, geo %in% c(eurostat_geos), time >= a_start_time) %>%
  droplevels() %>%
  complete(geo, time, nace_r2)


dat_eurostat_nace_imput <-
  dat_eurostat_nace %>%
  # Impute confidental 2015 for J in SE
  mutate_at(c("EMP_DC__THS_HW", "SAL_DC__THS_HW", "EMP_DC__THS_PER", "SAL_DC__THS_PER"),
            ~if_else(geo == "SE" & nace_r2 == "J" & time == 2015 & is.na(.),
                     mean(c(.[geo == "SE" & nace_r2 == "J" & time == 2014], .[geo == "SE" & nace_r2 == "J" & time == 2016])),
                     .))

dat_eurostat_nace10 <-
  dat_nama_10_a10 %>%
  left_join(dat_nama_10_a10_e, by = c("nace_r2", "geo", "time")) %>%
  mutate_at(c("geo", "nace_r2"), as_factor) %>%
  filter(nace_r2 %in% main_nace10_sna, geo %in% c(eurostat_geos), time >= a_start_time) %>%
  droplevels() %>%
  complete(geo, time, nace_r2)

dat_eurostat_nace10_long <-
  dat_nama_10_a10 %>%
  left_join(dat_nama_10_a10_e, by = c("nace_r2", "geo", "time")) %>%
  mutate_at(c("geo", "nace_r2"), as_factor) %>%
  filter(nace_r2 %in% main_nace10_sna, geo %in% c(eurostat_geos), time >= 1991) %>%
  droplevels() %>%
  complete(geo, time, nace_r2)

# visdat::vis_dat(dat_eurostat_nace10)

## Eurostat datasets

# With industries based on a64
data_eurostat_nama_nace_a <-
  dat_eurostat_nace_imput

# With industries based on a10
data_eurostat_nama_nace10_a <-
  dat_eurostat_nace10

# Total
data_eurostat_nama_a <-
  dat_nama_10_gdp %>%
  left_join(dat_eurostat_nace10, by = c("geo", "time", "nace_r2")) %>%
  filter(geo %in% eurostat_geos, time >= a_start_time) %>%
  mutate_if(is.character, as_factor)

data_eurostat_nama_a_long <-
  dat_nama_10_gdp %>%
  left_join(dat_eurostat_nace10_long, by = c("geo", "time", "nace_r2")) %>%
  filter(geo %in% eurostat_geos, time >= 1991) %>%
  complete(geo, time) %>%
  mutate_if(is.character, as_factor)

#
# setdiff(names(data_eurostat_nama_a, ), names(data_oecd_sna_a))
# setdiff(names(data_oecd_sna_a), names(data_eurostat_nama_a))

usethis::use_data(
  data_eurostat_nama_nace_a,
  data_eurostat_nama_nace10_a,
  data_eurostat_nama_a,
  data_eurostat_nama_a_long,
  overwrite = TRUE)
