

library(tidyr)
library(dplyr)
library(forcats)

load_all()

q_start_time <- "1996-01-01"
q_base_year <- 2010
a_start_time <- 1995
a_base_year <- 2010

## Countries

ea_countries <- eurostat::ea_countries$code
eu_countries<- eurostat::eu_countries$code
other_eurostat_countries <- c("NO", "CH", "IS")
agg_eurostat <- c("EA19", "EU28")

eurostat_geos <- c("BE", "DK", "DE", "IE", "ES", "FR", "IT", "NL", "AT", "FI", "SE", "UK")

#9 other industrial countries (Australia, Canada, United States, Japan, Norway, New Zealand, Mexico, Switzerland and Turkey)
# IC37_other <- c("AU", "CA", "US", "JP", "NO", "NZ", "MX", "CH", "TR")
# exec_countries <- c("CL", "CR", "IL", "IS", "KR")
# other_oecd <- c("AU", "CA", "US", "JP", "NO", "NZ", "CH")


oecd_geos <- c("US", "JP")



all_geos <- c(eurostat_geos, oecd_geos)

countries <- setNames(all_geos, countrycode::countrycode(all_geos, "eurostat", "cldr.name.fi",
                                                           custom_match = c(EA12 = "Euroalue-12")))


main_nace_sna <- c(VTOT = "TOTAL", VC = "C", V26 = "C26",  VF = "F", VG = "G", VH = "H",
                   VI = "I", VJ = "J", VM = "M", VN = "N")

# setdiff(unique(ulc_eurostat_dat$geo), eurostat::eu_countries$code)
#
# setdiff(eurostat::eu_countries$code, unique(stan_dat$geo))
#
# eurostat::label_eurostat(kk, dic = "geo")
# eurostat::label_eurostat(IC37_other, dic = "geo")

# oecd_geos %in% weights_bis_broad$geo

usethis::use_data(eurostat_geos, oecd_geos, main_nace_sna, a_start_time, a_base_year, overwrite = TRUE)

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
  B1G__CLV10_MEUR = "Value added volume in euros",
  B1G__CLV10_MNAC = "Value added volume in national currency",
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
  XGSVD = "",
  XMKT = ""
)

usethis::use_data(var_labels, overwrite = TRUE)

## Quarterly data for ULC
# Only for person based

# Data from eurostat
#  source("data-raw/eurostat.R")
#  in addition to EU countries also "NO", "CH", "IS"
data("naq_eurostat_dat")

q_dat_eurostat <- naq_eurostat_dat %>%
  unite(vars, na_item, unit, sep = "__") %>%
  filter(nace_r2 == "TOTAL", time >= q_start_time) %>%
  spread(vars, values) %>%
  group_by(geo) %>%
  transmute(
    time = time,
    gdp_ind = rebase(B1GQ__CLV10_MNAC, time = time, baseyear = q_base_year),
    exp_ind = rebase(P6__CLV10_MNAC, time = time, baseyear = q_base_year),
    # tbalance = B11__CP_MEUR,
    nulc_aper = ind_ulc(D1__CP_MNAC / SAL_DC__THS_PER, B1GQ__CLV10_MNAC / EMP_DC__THS_PER, time = time, baseyear = q_base_year)) %>%
  ungroup()

# and from OECD
#  source("data-raw/get_OECD.R")
data("ulc_oecd_dat")

q_dat_oecd_ulc <- ulc_oecd_dat %>%
  filter(time >= q_start_time, geo %in% oecd_geos) %>%
  spread(na_item, values) %>%
  group_by(geo) %>%
  transmute(
    time = time,
    nulc_aper = rebase(NULC_APER, time = time, baseyear = q_base_year)

  ) %>%
  ungroup()

q_dat_oecd <- oecd_dat_Q %>%
  unite(vars, na_item, unit, sep = "__") %>%
  filter(time >= q_start_time, geo %in% oecd_geos) %>%
  spread(vars, values) %>%
  group_by(geo) %>%
  transmute(
    time = time,
    gdp_ind = rebase(B1GQ__CLV_NAC, time = time, baseyear = q_base_year),
    exp_ind = rebase(P6__CLV_NAC, time = time, baseyear = q_base_year)
    # tbalance = B11__CP_MEUR,
    ) %>%
  ungroup() %>%
  left_join(q_dat_oecd_ulc, by = c("geo", "time"))

# combine
q_dat <-
  q_dat_eurostat %>%
  filter(geo %in% eurostat_geos) %>%
  bind_rows(filter(q_dat_oecd, geo %in% oecd_geos)) %>%
  group_by(time) %>%
  mutate(nulc_aper_rel15 = weight_index(nulc_aper, geo, 2015, weight_df = weights_bis_broad),
         nulc_aper_rel_bis = weight_index(nulc_aper, geo, lubridate::year(time), weight_df = weights_bis_broad),
         gdp_ind_rel15 = weight_index(gdp_ind, geo, 2015, weight_df = weights_bis_broad),
         exp_ind_rel15 = weight_index(exp_ind, geo, 2015, weight_df = weights_bis_broad)) %>%
  ungroup()


# q_dat %>%
#   group_by(geo) %>%
#   summarise_if(is.numeric, ~sum(is.na(.))) %>%
#   ungroup() %>%
#   filter((bkt_ind + exp_ind + nulc_aper) > 0)

# q_dat %>%
#   filter(geo == "FI") %>%
#   select(geo, time, gdp_ind, exp_ind, nulc_aper_rel15, gdp_ind_rel15, exp_ind_rel15) %>%
#   gather(vars, values, - geo, - time) %>%
#   ggplot(aes(time, values)) +
#   facet_wrap(~ vars, scales = "free_y") +
#   geom_line()

# visdat::vis_dat(q_dat)
# q_dat %>% filter(is.na(gdp_ind)) %>%distinct(geo)

usethis::use_data(q_dat, overwrite = TRUE)
write.csv2(q_dat, file = "data-out/ficomp_quarterly_data.csv")
saveRDS(q_dat, file = "data-out/ficomp_quarterly_data.rds")






## Annual data

data("stan_dat")
data("data_oecd_sna_a")

a_dat_oecd <- data_oecd_sna_a %>%
  filter(time >= a_start_time,
         geo %in% oecd_geos) %>%
  group_by(geo) %>%
  transmute(
    time = time,
    bkt_ind = rebase(B1GQ__CLV15_MNAC, time = time, baseyear = a_base_year),
    exp_ind = rebase(P6__CLV15_MNAC, time = time, baseyear = a_base_year)
    # tbalance = B11__CP_MEUR,
  ) %>%
  ungroup()

# visdat::vis_dat(a_dat_oecd)

a_dat_ulc_oecd <-
  stan_dat %>%
  filter(time >= a_start_time,
         geo %in% oecd_geos,
         nace_r2 %in% c("TOTAL", "C")) %>%
  mutate(nulc_aper_va = ind_ulc(D1__CP_MNAC / SAL_DC__THS_PER, B1G__CLV10_MNAC / EMP_DC__THS_PER, time = time, baseyear = a_base_year),
         nulc_hw_va = ind_ulc(D1__CP_MNAC / SAL_DC__THS_HW, B1G__CLV10_MNAC / EMP_DC__THS_HW, time = time, baseyear = a_base_year))

# visdat::vis_dat(a_dat_ulc_oecd)

data("dat_nama_10_gdp", "dat_nama_10_a64")

a_dat_dep <-
  dat_nama_10_gdp %>%
  filter(geo %in% eurostat_geos,
         time >= a_start_time) %>%
  droplevels() %>%
  complete(geo, time) %>%
  group_by(geo) %>%
  transmute(
    time = time,
    gdp_ind = rebase(B1GQ__CLV15_MNAC, time = time, baseyear = a_base_year),
    exp_ind = rebase(P6__CLV15_MNAC, time = time, baseyear = a_base_year),
    tbalance_gdp = 100 * B11__CP_MNAC / B1GQ__CP_MNAC,
  ) %>%
  complete(geo, time) %>%
  group_by(time) %>%
  mutate(gdp_ind_rel = weight_index(gdp_ind, geo, time, weight_df = weights_bis_broad),
         exp_ind_rel = weight_index(exp_ind, geo, time, weight_df = weights_bis_broad)) %>%
  ungroup()


a_dat_ind <-
  dat_nama_10_a64 %>%
  filter(geo %in% eurostat_geos,
         time >= a_start_time) %>%
  droplevels() %>%
  complete(geo, time, nace_r2) %>%
  group_by(geo, nace_r2) %>%
  mutate(nulc_aper_va = ind_ulc(D1__CP_MNAC / SAL_DC__THS_PER, B1G__CLV10_MNAC / EMP_DC__THS_PER, time = time, baseyear = a_base_year),
         nulc_hw_va = ind_ulc(D1__CP_MNAC / SAL_DC__THS_HW, B1G__CLV10_MNAC / EMP_DC__THS_HW, time = time, baseyear = a_base_year),
         nulc_hw_va_eur = ind_ulc(D1__CP_MEUR / SAL_DC__THS_HW, B1G__CLV10_MNAC / EMP_DC__THS_HW, time = time, baseyear = a_base_year),
         rulc_hw_va = rebase(nulc_hw_va / (B1G__CP_MNAC/B1G__CLV10_MNAC), time = time, baseyear = a_base_year)) %>%
  group_by(nace_r2, time) %>%
  mutate(nulc_hw_va_rel = weight_index(nulc_hw_va, geo, time, weight_df = weights_bis_broad),
         nulc_hw_va_eur_rel = weight_index(nulc_hw_va_eur, geo, time, weight_df = weights_bis_broad),
         rulc_hw_va_rel = weight_index(rulc_hw_va, geo, time, weight_df = weights_bis_broad)) %>%
  ungroup()

a_dat_ind_oecd <-
  dat_nama_10_a64 %>%
  filter(geo %in% eurostat_geos,
         time >= a_start_time) %>%
  group_by(geo, nace_r2) %>%
  mutate(nulc_aper_va = ind_ulc(D1__CP_MNAC / SAL_DC__THS_PER, B1G__CLV10_MNAC / EMP_DC__THS_PER, time = time, baseyear = a_base_year),
         nulc_hw_va = ind_ulc(D1__CP_MNAC / SAL_DC__THS_HW, B1G__CLV10_MNAC / EMP_DC__THS_HW, time = time, baseyear = a_base_year),
         nulc_hw_va_eur = ind_ulc(D1__CP_MEUR / SAL_DC__THS_HW, B1G__CLV10_MNAC / EMP_DC__THS_HW, time = time, baseyear = a_base_year),
         rulc_hw_va = rebase(nulc_hw_va / (B1G__CP_MNAC/B1G__CLV10_MNAC), time = time, baseyear = a_base_year)) %>%
  group_by(nace_r2, time) %>%
  mutate(nulc_hw_va_rel = weight_index(nulc_hw_va, geo, time, weight_df = weights_bis_broad),
         nulc_hw_va_eur_rel = weight_index(nulc_hw_va_eur, geo, time, weight_df = weights_bis_broad),
         rulc_hw_va_rel = weight_index(rulc_hw_va, geo, time, weight_df = weights_bis_broad)) %>%
  ungroup()


# a_dat_dep %>%
#   filter(geo == "FI") %>%
#   select(geo, time, gdp_ind, exp_ind, gdp_ind_rel15, exp_ind_rel15, tbalance_gdp) %>%
#   gather(vars, values, - geo, - time) %>%
#   ggplot(aes(time, values)) +
#   facet_wrap(~ vars, scales = "free_y") +
#   geom_line()

# a_dat_ind %>%
#   filter(nace_r2 == "TOTAL") %>%
#   filter(is.na(B1G__CLV10_MEUR)) %>% distinct(geo)

# visdat::vis_dat(a_dat_dep)


# Combine

a_dat <-
  a_dat_dep %>%
  right_join(a_dat_ind, by = c("geo", "time")) %>%
  mutate(geo = as_factor(geo))

usethis::use_data(a_dat, overwrite = TRUE)
write.csv2(a_dat, file = "data-out/ficomp_annual_data.csv")
saveRDS(a_dat, file = "data-out/ficomp_annual_data.rds")



## Final annual data

data_main_nace_a <-
  data_eurostat_nama_nace_a %>%
  bind_rows(select(data_oecd_sna_nace_a, all_of(names(.)))) %>%
  # filter(time >= start_time_main,
  #        geo %in% countries) %>%
  mutate(geo_name = fct_recode(geo, !!!countries),
         geo = as_factor(geo))

# visdat::vis_dat(data_main_nace_a)

data_main_groups_a <-
  data_main_nace_a %>%
  gather(vars, values, -time, - geo, -geo_name, - nace_r2) %>%
  group_by(geo, geo_name, time, vars) %>%
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
  group_by(geo, geo_name, nace0) %>%
  # volyymia ei voi laskea yhteen, täytyy laske cp ja pp sarjoista
  mutate(B1G__CLV15_MNAC = statfitools::fp(cp = B1G__CP_MNAC, pp = B1G__PYP_MNAC, time = time, year = 2015)) %>%
  ungroup() %>%
  mutate(geo_name = fct_relevel(geo_name, rev(names(countries)))) %>%
  group_by(geo, nace0) %>%
  mutate(nulc_aper_va = ind_ulc(D1__CP_MNAC / SAL_DC__THS_PER, B1G__CLV15_MNAC / EMP_DC__THS_PER, time = time, baseyear = a_base_year),
         nulc_hw_va = ind_ulc(D1__CP_MNAC / SAL_DC__THS_HW, B1G__CLV15_MNAC / EMP_DC__THS_HW, time = time, baseyear = a_base_year),
         nulc_hw_va_eur = ind_ulc(D1__CP_MEUR / SAL_DC__THS_HW, B1G__CLV15_MNAC / EMP_DC__THS_HW, time = time, baseyear = a_base_year),
         rulc_hw_va = rebase(nulc_hw_va / (B1G__CP_MNAC/B1G__CLV15_MNAC), time = time, baseyear = a_base_year)) %>%
  group_by(nace0, time) %>%
  mutate(nulc_hw_va_rel = weight_index(nulc_hw_va, geo, time, weight_df = weights_bis_broad),
         nulc_hw_va_eur_rel = weight_index(nulc_hw_va_eur, geo, time, weight_df = weights_bis_broad),
         rulc_hw_va_rel = weight_index(rulc_hw_va, geo, time, weight_df = weights_bis_broad),
         nulc_hw_va_rel_imf = weight_index(nulc_hw_va, geo, time, weight_df = weights_imf),
         nulc_hw_va_eur_rel_imf = weight_index(nulc_hw_va_eur, geo, time, weight_df = weights_imf),
         rulc_hw_va_rel_imf = weight_index(rulc_hw_va, geo, time, weight_df = weights_imf)) %>%
  ungroup()

data_main_total_a <-
  data_eurostat_nama_a %>%
  bind_rows(select(data_oecd_sna_a, all_of(names(.)))) %>%
  # filter(time >= start_time_main,
  #        geo %in% countries) %>%
  mutate(geo_name = fct_recode(geo, !!!countries),
         geo = as_factor(geo)) %>%
  select(- nace_r2) %>%
  left_join(select(filter(eo_a_dat, time >= a_start_time), geo, time, XPERF, XSHA, XGSVD, XMKT, eci, nulc_eo = nulc), by = c("geo", "time")) %>%
  group_by(geo) %>%
  mutate(
    gdp_ind = rebase(B1GQ__CLV15_MNAC, time = time, baseyear = a_base_year),
    exp_ind = rebase(P6__CLV15_MNAC, time = time, baseyear = a_base_year),
    tbalance_gdp = B11__CP_MNAC / B1GQ__CP_MNAC) %>%
  ungroup() %>%
  group_by(time) %>%
  mutate(gdp_ind_rel = weight_index(gdp_ind, geo, time, weight_df = weights_bis_broad),
         exp_ind_rel = weight_index(exp_ind, geo, time, weight_df = weights_bis_broad),
         gdp_ind_rel_imf = weight_index(gdp_ind, geo, time, weight_df = weights_imf),
         exp_ind_rel_imf = weight_index(exp_ind, geo, time, weight_df = weights_imf)) %>%
  ungroup()

# visdat::vis_dat(data_main_total_a)
#




usethis::use_data(data_main_groups_a, data_main_total_a, overwrite = TRUE)
write.csv2(data_main_groups_a, file = "data-out/data_main_groups_annual.csv")
saveRDS(data_main_groups_a, file = "data-out/data_main_groups_annual.rds")

write.csv2(data_main_total_a, file = "data-out/data_main_total_annual.csv")
saveRDS(data_main_total_a, file = "data-out/data_main_total_annual.rds")
