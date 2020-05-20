
# Setup

library(tidyr)
library(dplyr)
library(forcats)

devtools::load_all()

base_year <- 2010
q_start_time <- "1996-01-01"
a_start_time <- 1995




## Countries

ea_geo <- eurostat::ea_countries$code
eu_geo <- eurostat::eu_countries$code
other_eurostat_geo <- c("NO", "CH", "IS")
agg_eurostat <- c("EA19", "EU28")

eurostat_geos <- c("BE", "DK", "DE", "IE", "ES", "FR", "IT", "NL", "AT", "FI", "SE", "UK", "NO", "PT", "EL")

#9 other industrial countries (Australia, Canada, United States, Japan, Norway, New Zealand, Mexico, Switzerland and Turkey)
# IC37_other <- c("AU", "CA", "US", "JP", "NO", "NZ", "MX", "CH", "TR")
# exec_countries <- c("CL", "CR", "IL", "IS", "KR")
# other_oecd <- c("AU", "CA", "US", "JP", "NO", "NZ", "CH")

oecd_geos_ulcq <- c("AU", "CA", "US", "JP", "NO", "NZ")
oecd_geos <- c("US", "JP")

ameco_extra_geos <- c("AU", "CA", "US", "JP", "NZ", "CH")

all_geos <- c(eurostat_geos, oecd_geos)

all_extra_geos <- c(eu_geo, other_eurostat_geo, c("AU", "CA", "US", "JP", "NO", "NZ", "MX", "CH", "TR", "IL", "KR"))

geo_fi <- setNames(countrycode::countrycode(all_extra_geos, "eurostat", "cldr.name.fi",
                                            custom_match = c(EA12 = "Euroalue-12")), all_extra_geos)

nace0_fi <- c(total = "Koko talous",
              manu = "Teollisuus",
              private = "Yksityinen",
              private_ex26 = "Yksityinen pl. 26",
              manu_ex26 = "Teollisuus pl. 26",
              service = "Palvelut")


main_nace_sna <- c(VTOT = "TOTAL", VC = "C", V26 = "C26",  VF = "F", VG = "G", VH = "H",
                   VI = "I", VJ = "J", VM = "M", VN = "N")

main_nace10_sna <- c(TOTAL = "TOTAL", C = "C", F = "F", G = "G-I", H = "G-I",
                     I = "G-I", J = "J", M = "M_N", N = "M_N")

usethis::use_data(eurostat_geos, oecd_geos_ulcq, oecd_geos, all_geos, all_extra_geos, main_nace_sna,
                  main_nace10_sna, a_start_time, base_year, overwrite = TRUE)
usethis::use_data(nace0_fi, geo_fi, overwrite = TRUE)

# Variables used

var_labels <- c(
  geo = "Countries",
  time = "time",
  gdp_ind = "GDP volume index",
  exp_ind = "Export of goods and services volume index",
  tbalance_gdp = "Trade and services balace as share of GDP",
  gdp_ind_rel15 = "GDP volume index related to other countries",
  exp_ind_rel15 = "Export volume index related to other countries, 2014-2016  double trade weights",
  nace_r2 = "Industry classification",
  B1G__CLV15_MEUR = "Value added volume in euros",
  B1G__CLV15_MNAC = "Value added volume in national currency",
  B1G__CP_MEUR = "Value added in current prices in euros",
  D1__CP_MEUR = "Compensation of employees in euros",
  B1G__CP_MNAC = "Value added in current prices in national currency",
  D1__CP_MNAC = "Compensation of employees in national currency",
  B1G__PYP_MNAC = "Value added in previous years prices in national currency",
  EMP_DC__THS_HW = "Total hours of employed domestic concept, thousand",
  SAL_DC__THS_HW = "Total hours of employees domestic concept, thousand",
  EMP_DC__THS_PER = "Total employment domestic concept, thousand persons",
  SAL_DC__THS_PER = "Employees domestic concept, thousand persons",
  nulc_aper_va = "Nominal unit labour cost, persons, value added",
  nulc_hw_va = "Nominal unit labour cost, hours, value added",
  nulc_hw_va_eur = "Nominal unit labour cost, hours, value added, in common currency (EUR)",
  rulc_hw_va = "Real unit labour cost, hours, value added",
  nulc_hw_va_rel = "Relative nominal unit labour cost, hours, value added, related to other countries, rolling  double trade weights",
  nulc_hw_va_eur_rel = "Relative nominal unit labour cost, hours, value added, in common currency (EUR), related to other countries, rolling  double trade weights",
  rulc_hw_va_rel = "Relative real unit labour cost, hours, value added, related to other countries, rolling double trade weights",
  XPERF = "Export performance for goods and services, volume",
  XSHA = "Share of value exports of goods and services in world exports in USD",
  XGSVD = "Exports of goods and services, volume, USD, 2005 prices",
  XMKT = "Export market for goods and services, volume, USD, 2005 prices"
)

var_labels_fi <- c(
  geo = "Countries",
  time = "time",
  gdp_ind = "GDP volume index",
  exp_ind = "Export of goods and services volume index",
  tbalance_gdp = "Trade and services balace as share of GDP",
  gdp_ind_rel15 = "GDP volume index related to other countries",
  exp_ind_rel15 = "Export volume index related to other countries, 2014-2016  double trade weights",
  nace_r2 = "Industry classification",
  B1GQ__CLV15_MNAC = "GDP volume in national currency",
  B1GQ__CP_MNAC = "GDP in current prices in national currency",
  B1GQ__CP_MEUR = "GDP in current prices in euros",
  B1GQ__PYP_MNAC = "GDP in previous years prices in national currency",
  B1G__CLV15_MNAC = "Value added volume in national currency",
  B1G__CP_MNAC = "Value added in current prices in national currency",
  B1G__CP_MEUR = "Value added in current prices in euros",
  B1G__PYP_MNAC = "Value added in previous years prices in national currency",
  P6__CLV15_MNAC = "Export of goods and services volume in national currency",
  P6__CP_MNAC = "Export of goods and services in current prices in national currency",
  P6__CP_MEUR = "Export of goods and services in current prices in euros",
  P61__CLV15_MNAC = "Export ofgoods volume in national currency",
  P61__CP_MNAC = "Export ofgoods in current prices in national currency",
  P61__CP_MEUR = "Export ofgoods in current prices in euros",
  P62__CLV15_MNAC = "Export ofservices volume in national currency",
  P62__CP_MNAC = "Export ofservices in current prices in national currency",
  P62__CP_MEUR = "Export ofservices in current prices in euros",
  D1__CP_MNAC = "Compensation of employees in national currency",
  D1__CP_MEUR = "Compensation of employees in euros",
  B11__CP_MNAC = "External balance of goods and services in national currency",
  B11__CP_MEUR = "External balance of goods and services in euros",
  EMP_DC__THS_HW = "Total hours of employed domestic concept, thousand",
  SAL_DC__THS_HW = "Total hours of employees domestic concept, thousand",
  EMP_DC__THS_PER = "Total employment domestic concept, thousand persons",
  SAL_DC__THS_PER = "Employees domestic concept, thousand persons",
  nulc = "Nominal unit labour cost",
  nulc_aper_va = "Nominal unit labour cost, persons, value added",
  nulc_hw_va = "Nominal unit labour cost, hours, value added",
  nulc_hw_va_eur = "Nominal unit labour cost, hours, value added, in common currency (EUR)",
  rulc_hw_va = "Real unit labour cost, hours, value added",
  nulc_hw_va_rel = "Relative nominal unit labour cost, hours, value added, related to other countries, rolling  double trade weights",
  nulc_hw_va_eur_rel = "Relative nominal unit labour cost, hours, value added, in common currency (EUR), related to other countries, rolling  double trade weights",
  rulc_hw_va_rel = "Relative real unit labour cost, hours, value added, related to other countries, rolling double trade weights",
  XPERF = "Export performance for goods and services, volume",
  XSHA = "Share of value exports of goods and services in world exports in USD",
  XGSVD = "Exports of goods and services, volume, USD, 2005 prices",
  XMKT = "Export market for goods and services, volume, USD, 2005 prices"
)

usethis::use_data(var_labels, overwrite = TRUE)




## Quarterly data for Total economy

# OECD ULC

#  source("data-raw/get_OECD.R")
data("ulc_oecd_dat")

q_dat_oecd_ulc <- ulc_oecd_dat %>%
  filter(time >= q_start_time, geo %in% c(eurostat_geos, oecd_geos_ulcq)) %>%
  spread(na_item, values) %>%
  group_by(geo) %>%
  transmute(
    time = time,
    nulc_aper = rebase(NULC_APER, time = time, baseyear = base_year)
  ) %>%
  group_by(time) %>%
  mutate(nulc_aper_rel_imf = weight_index(nulc_aper, geo, lubridate::year(time), weight_df = weights_imf)) %>%
  ungroup()


# OECD QNA

q_dat_oecd <- oecd_dat_Q %>%
  unite(vars, na_item, unit, sep = "__") %>%
  filter(time >= q_start_time, geo %in% oecd_geos) %>%
  mutate(nace_r2 = "TOTAL") %>%
  spread(vars, values) %>%
  # To same base year as eurostat
  group_by(geo) %>%
  mutate_at(vars(matches("CLV_")), ~rebase(., time = time,  baseyear = base_year, basevalue = NULL)) %>%
  ungroup() %>%
  rename_at(vars(matches("CLV_")), ~gsub("CLV_", "CLV15_", .))  %>%
  # euro current price series
  mutate_at(vars(matches("CP_MNAC")),
            .funs = list(EUR = ~EUR(., time, currency, exh_eur_q))) %>%
  rename_at(vars(contains("MNAC_EUR")), list(~gsub("NAC_", "", .)))%>%
  select(-currency)


# Eurostat QNA
# Data from eurostat
#  source("data-raw/eurostat.R")
#  in addition to EU countries also "NO", "CH", "IS"
data("naq_eurostat_dat")

q_dat_eurostat <- naq_eurostat_dat %>%
  unite(vars, na_item, unit, sep = "__") %>%
  filter(nace_r2 == "TOTAL", time >= q_start_time) %>%
  spread(vars, values)



# Combine Eurostat and OECD QNA
q_dat <-
  q_dat_eurostat %>%
  filter(geo %in% eurostat_geos) %>%
  bind_rows(filter(q_dat_oecd, geo %in% oecd_geos)) %>%
  # join nulc_aper from OECD ulc and replace nulc if missing (in mutate)
  left_join(select(q_dat_oecd_ulc, geo, time, nulc_aper_oecd = nulc_aper),
            by = c("geo", "time")) %>%
  group_by(geo) %>%
  mutate(
    nulc = ind_ulc(D1__CP_MNAC, B1GQ__CLV15_MNAC, time = time, baseyear = base_year),
    nulc_eur = ind_ulc(D1__CP_MEUR, B1GQ__CLV15_MNAC, time = time, baseyear = base_year),
    nulc_va = ind_ulc(D1__CP_MNAC, B1G__CLV15_MNAC, time = time, baseyear = base_year),
    nulc_aper = ind_ulc(D1__CP_MNAC / SAL_DC__THS_PER, B1GQ__CLV15_MNAC / EMP_DC__THS_PER, time = time, baseyear = base_year),
    nulc_aper = coalesce(nulc_aper, nulc_aper_oecd),
    nulc_aper_va = ind_ulc(D1__CP_MNAC / SAL_DC__THS_PER, B1G__CLV15_MNAC / EMP_DC__THS_PER, time = time, baseyear = base_year),
    nulc_hw = ind_ulc(D1__CP_MNAC / SAL_DC__THS_HW, B1GQ__CLV15_MNAC / EMP_DC__THS_HW, time = time, baseyear = base_year),
    nulc_hw_va = ind_ulc(D1__CP_MNAC / SAL_DC__THS_HW, B1G__CLV15_MNAC / EMP_DC__THS_HW, time = time, baseyear = base_year),
    nulc_hw_va_eur = ind_ulc(D1__CP_MEUR / SAL_DC__THS_HW, B1G__CLV15_MNAC / EMP_DC__THS_HW, time = time, baseyear = base_year),
    rulc_aper = rebase(nulc_aper / (B1GQ__CP_MNAC/B1GQ__CLV15_MNAC), time = time, baseyear = base_year),
    rulc_hw_va = rebase(nulc_hw_va / (B1GQ__CP_MNAC/B1GQ__CLV15_MNAC), time = time, baseyear = base_year)
    ) %>%
  mutate(
    gdp_ind = rebase(B1GQ__CLV15_MNAC, time = time, baseyear = base_year),
    exp_ind = rebase(P6__CLV15_MNAC, time = time, baseyear = base_year),
    exp_goods_ind = rebase(P61__CLV15_MNAC, time = time, baseyear = base_year),
    exp_serv_ind = rebase(P62__CLV15_MNAC, time = time, baseyear = base_year),
    tbalance_gdp = B11__CP_MNAC / B1GQ__CP_MNAC) %>%
  group_by(time) %>%
  mutate(
    nulc_aper_rel15 = weight_index(nulc_aper, geo, 2015, weight_df = weights_bis_broad),
    nulc_aper_rel_bis = weight_index(nulc_aper, geo, lubridate::year(time), weight_df = weights_bis_broad),
    nulc_aper_rel_imf = weight_index(nulc_aper, geo, lubridate::year(time), weight_df = weights_imf),
    nulc_rel_imf = weight_index(nulc, geo, lubridate::year(time), weight_df = weights_imf),
    nulc_eur_rel_imf = weight_index(nulc_eur, geo, lubridate::year(time), weight_df = weights_imf),
    rulc_aper_imf = weight_index(rulc_aper, geo, time, weight_df = weights_imf),
    gdp_ind_rel_imf = weight_index(gdp_ind, geo, time, weight_df = weights_imf),
    exp_ind_rel_imf = weight_index(exp_ind, geo, time, weight_df = weights_imf),
    exp_goods_ind_rel_imf = weight_index(exp_goods_ind, geo, time, weight_df = weights_imf),
    exp_serv_ind_rel_imf = weight_index(exp_serv_ind, geo, time, weight_df = weights_imf)) %>%
  ungroup() %>%
  left_join(select(filter(eo_q_dat, time >= q_start_time), geo, time, XPERF, XGSVD, XMKT, eci, nulc_eo = nulc), by = c("geo", "time"))



usethis::use_data(q_dat, q_dat_oecd_ulc, overwrite = TRUE)
write.csv2(q_dat, file = "data-out/data_main_total_quarterly.csv")
saveRDS(q_dat, file = "data-out/data_main_total_quarterly.rds")


## Quarterly industry group data

data_eurostat_nace_q <- naq_eurostat_nace_dat %>%
  unite(vars, na_item, unit, sep = "__") %>%
  filter(time >= q_start_time) %>%
  spread(vars, values)

# visdat::vis_dat(data_eurostat_nace_q)


data_main_nace_q <-
  data_eurostat_nace_q %>%
  filter(geo != "BE") %>%
  # bind_rows(select(data_oecd_sna_nace_q, all_of(names(.)))) %>%
  # filter(time >= start_time_main,
  #        geo %in% countries) %>%
  mutate(geo = as_factor(geo))

# Take only total and C. For other need to calculate PYP

data_main_groups_q <-
  data_main_nace_q %>%
  filter(nace_r2 %in% c("TOTAL", "C")) %>%
  mutate(nace0 = fct_recode(nace_r2, total = "TOTAL", manu = "C")) %>%
  select(-nace_r2) %>%
  group_by(geo, nace0) %>%
  mutate(nulc_aper_va = ind_ulc(D1__CP_MNAC / SAL_DC__THS_PER, B1G__CLV15_MNAC / EMP_DC__THS_PER, time = time, baseyear = base_year),
         nulc_hw_va = ind_ulc(D1__CP_MNAC / SAL_DC__THS_HW, B1G__CLV15_MNAC / EMP_DC__THS_HW, time = time, baseyear = base_year),
         nulc_hw_va_eur = ind_ulc(D1__CP_MEUR / SAL_DC__THS_HW, B1G__CLV15_MNAC / EMP_DC__THS_HW, time = time, baseyear = base_year),
         rulc_hw_va = rebase(nulc_hw_va / (B1G__CP_MNAC/B1G__CLV15_MNAC), time = time, baseyear = base_year)) %>%
  group_by(nace0, time) %>%
  mutate(nulc_hw_va_rel = weight_index(nulc_hw_va, geo, time, weight_df = weights_bis_broad),
         nulc_hw_va_eur_rel = weight_index(nulc_hw_va_eur, geo, time, weight_df = weights_bis_broad),
         rulc_hw_va_rel = weight_index(rulc_hw_va, geo, time, weight_df = weights_bis_broad),
         nulc_aper_va_rel_imf = weight_index(nulc_aper_va, geo, time, weight_df = weights_imf),
         nulc_hw_va_eur_rel_imf = weight_index(nulc_hw_va_eur, geo, time, weight_df = weights_imf),
         rulc_hw_va_rel_imf = weight_index(rulc_hw_va, geo, time, weight_df = weights_imf)) %>%
  ungroup()



usethis::use_data(data_main_groups_q, overwrite = TRUE)
write.csv2(data_main_groups_q, file = "data-out/data_main_groups_quarterly.csv")
saveRDS(data_main_groups_q, file = "data-out/data_main_groups_quarterly.rds")






## Final annual data

data("data_eurostat_nama_nace_a", "data_oecd_sna_nace_a")

# Compine Eurostat and OECD annual data

data_main_nace_a <-
  data_eurostat_nama_nace_a %>%
  bind_rows(select(data_oecd_sna_nace_a, all_of(names(.)))) %>%
  # filter(time >= start_time_main,
  #        geo %in% countries) %>%
  mutate(geo = as_factor(geo))


# visdat::vis_dat(data_main_nace_a)

data_main_groups_a <-
  data_main_nace_a %>%
  gather(vars, values, -time, - geo, - nace_r2) %>%
  group_by(geo, time, vars) %>%
  summarise(
    total = values[nace_r2 == "TOTAL"],
            private = sum(values[!(nace_r2 %in% c("TOTAL", "C26"))]),
            private_ex26 = private - values[nace_r2 == "C26"],
            manu = sum(values[nace_r2 == "C"]),
            manu_ex26 = manu - values[nace_r2 == "C26"],
            service = sum(values[nace_r2 %in% c(c("G", "H", "I", "J", "M", "N"))])) %>%
  ungroup() %>%
  gather(nace0, values, total, private, private_ex26, manu, manu_ex26, service) %>%
  mutate(nace0 = as_factor(nace0)) %>%
  spread(vars, values) %>%
  group_by(geo, nace0) %>%
  # volyymia ei voi laskea yhteen, täytyy laske cp ja pp sarjoista
  mutate(B1G__CLV15_MNAC = statfitools::fp(cp = B1G__CP_MNAC, pp = B1G__PYP_MNAC, time = time, year = 2015)) %>%
  ungroup() %>%
  group_by(geo, nace0) %>%
  mutate(
    nulc_va = ind_ulc(D1__CP_MNAC, B1G__CLV15_MNAC, time = time, baseyear = base_year),
    nulc_va_eur = ind_ulc(D1__CP_MEUR, B1G__CLV15_MNAC, time = time, baseyear = base_year),
    nulc_aper_va = ind_ulc(D1__CP_MNAC / SAL_DC__THS_PER, B1G__CLV15_MNAC / EMP_DC__THS_PER, time = time, baseyear = base_year),
    nulc_hw_va = ind_ulc(D1__CP_MNAC / SAL_DC__THS_HW, B1G__CLV15_MNAC / EMP_DC__THS_HW, time = time, baseyear = base_year),
    nulc_hw_va_eur = ind_ulc(D1__CP_MEUR / SAL_DC__THS_HW, B1G__CLV15_MNAC / EMP_DC__THS_HW, time = time, baseyear = base_year),
    rulc_hw_va = rebase(nulc_hw_va / (B1G__CP_MNAC/B1G__CLV15_MNAC), time = time, baseyear = base_year)) %>%
  group_by(nace0, time) %>%  #filter(nace0 == "total", time == 2015) -> kk
  mutate(
    nulc_va_rel_imf = weight_index(nulc_va, geo, time, weight_df = weights_imf),
    nulc_va_eur_rel_imf = weight_index(nulc_va_eur, geo, time, weight_df = weights_imf),
    nulc_va_rel15_imf = weight_index(nulc_va, geo, 2015, weight_df = weights_imf),
    nulc_va_eur_rel15_imf = weight_index(nulc_va_eur, geo, 2015, weight_df = weights_imf),
    nulc_hw_va_rel = weight_index(nulc_hw_va, geo, time, weight_df = weights_bis_broad),
    nulc_hw_va_eur_rel = weight_index(nulc_hw_va_eur, geo, time, weight_df = weights_bis_broad),
    rulc_hw_va_rel = weight_index(rulc_hw_va, geo, time, weight_df = weights_bis_broad),
    nulc_hw_va_rel_imf = weight_index(nulc_hw_va, geo, time, weight_df = weights_imf),
    nulc_hw_va_eur_rel_imf = weight_index(nulc_hw_va_eur, geo, time, weight_df = weights_imf),
    rulc_hw_va_rel_imf = weight_index(rulc_hw_va, geo, time, weight_df = weights_imf)) %>%
  ungroup()

data("data_eurostat_nama_a")

data_main_total_a <-
  data_eurostat_nama_a %>%
  # Following are missing from OECD
  select( -B1G__CLV15_MNAC, -B1G__CP_MNAC, -B1G__PYP_MNAC,
          -EMP_DC__THS_HW, -SAL_DC__THS_HW,
          -EMP_DC__THS_PER, -SAL_DC__THS_PER) %>%
  bind_rows(select(data_oecd_sna_a, all_of(names(.)))) %>%
  select(- nace_r2) %>%
  mutate(geo = as_factor(geo)) %>%
  left_join(select(filter(eo_a_dat, time >= a_start_time), geo, time, XPERF, XSHA, XGSVD, XMKT, eci, nulc_eo = nulc), by = c("geo", "time")) %>%
  group_by(geo) %>%
  mutate(
    gdp_ind = rebase(B1GQ__CLV15_MNAC, time = time, baseyear = base_year),
    exp_ind = rebase(P6__CLV15_MNAC, time = time, baseyear = base_year),
    exp_goods_ind = rebase(P61__CLV15_MNAC, time = time, baseyear = base_year),
    exp_serv_ind = rebase(P62__CLV15_MNAC, time = time, baseyear = base_year),
    tbalance_gdp = B11__CP_MNAC / B1GQ__CP_MNAC) %>%
  ungroup() %>%
  group_by(time) %>%
  mutate(
    gdp_ind_rel = weight_index(gdp_ind, geo, time, weight_df = weights_bis_broad),
         exp_ind_rel = weight_index(exp_ind, geo, time, weight_df = weights_bis_broad),
         gdp_ind_rel_imf = weight_index(gdp_ind, geo, time, weight_df = weights_imf),
         exp_ind_rel_imf = weight_index(exp_ind, geo, time, weight_df = weights_imf),
         exp_goods_ind_rel_imf = weight_index(exp_goods_ind, geo, time, weight_df = weights_imf),
         exp_serv_ind_rel_imf = weight_index(exp_serv_ind, geo, time, weight_df = weights_imf)) %>%
  ungroup()

# visdat::vis_dat(data_main_total_a)
#

data_main_annual <-
  data_eurostat_nama_a %>%
  # Ameco is missing following
  select(- nace_r2, - B11__CP_MEUR, -B1G__PYP_MNAC, -EMP_DC__THS_HW, -SAL_DC__THS_HW) %>%
  bind_rows(select(filter(data_ameco, geo %in% ameco_extra_geos), all_of(names(.)))) %>%
  mutate(geo = as_factor(geo)) %>%
  left_join(select(filter(eo_a_dat, time >= a_start_time), geo, time, XPERF, XSHA, XGSVD, XMKT, eci, nulc_eo = nulc), by = c("geo", "time")) %>%
  group_by(geo) %>%
  mutate(
    nulc = ind_ulc(D1__CP_MNAC, B1GQ__CLV15_MNAC, time = time, baseyear = base_year),
    nulc_eur = ind_ulc(D1__CP_MEUR, B1GQ__CLV15_MNAC, time = time, baseyear = base_year),
    nulc_va = ind_ulc(D1__CP_MNAC, B1G__CLV15_MNAC, time = time, baseyear = base_year),
    nulc_aper = ind_ulc(D1__CP_MNAC / SAL_DC__THS_PER, B1GQ__CLV15_MNAC / EMP_DC__THS_PER, time = time, baseyear = base_year),
    nulc_aper_va = ind_ulc(D1__CP_MNAC / SAL_DC__THS_PER, B1G__CLV15_MNAC / EMP_DC__THS_PER, time = time, baseyear = base_year),
    rulc_aper = rebase(nulc_aper / (B1GQ__CP_MNAC/B1GQ__CLV15_MNAC), time = time, baseyear = base_year),
    gdp_ind = rebase(B1GQ__CLV15_MNAC, time = time, baseyear = base_year),
    exp_ind = rebase(P6__CLV15_MNAC, time = time, baseyear = base_year),
    exp_goods_ind = rebase(P61__CLV15_MNAC, time = time, baseyear = base_year),
    exp_serv_ind = rebase(P62__CLV15_MNAC, time = time, baseyear = base_year),
    tbalance_gdp = B11__CP_MNAC / B1GQ__CP_MNAC) %>%
  ungroup() %>%
  group_by(time) %>%
  mutate(
    gdp_ind_rel = weight_index(gdp_ind, geo, time, weight_df = weights_bis_broad),
    exp_ind_rel = weight_index(exp_ind, geo, time, weight_df = weights_bis_broad),
    gdp_ind_rel_imf = weight_index(gdp_ind, geo, time, weight_df = weights_imf),
    exp_ind_rel_imf = weight_index(exp_ind, geo, time, weight_df = weights_imf),
    exp_goods_ind_rel_imf = weight_index(exp_goods_ind, geo, time, weight_df = weights_imf),
    exp_serv_ind_rel_imf = weight_index(exp_serv_ind, geo, time, weight_df = weights_imf)) %>%
  ungroup()



usethis::use_data(data_main_groups_a, data_main_total_a, data_main_annual, overwrite = TRUE)
write.csv2(data_main_groups_a, file = "data-out/data_main_groups_annual.csv")
saveRDS(data_main_groups_a, file = "data-out/data_main_groups_annual.rds")

write.csv2(data_main_total_a, file = "data-out/data_main_total_annual.csv")
saveRDS(data_main_total_a, file = "data-out/data_main_total_annual.rds")

