
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

tuku16 <- c("BE", "DK", "DE", "ES", "FR", "IT", "NL", "AT", "FI", "SE", "UK", "NO", "US", "JP", "KR", "CA")

#9 other industrial countries (Australia, Canada, United States, Japan, Norway, New Zealand, Mexico, Switzerland and Turkey)
# IC37_other <- c("AU", "CA", "US", "JP", "NO", "NZ", "MX", "CH", "TR")
# exec_countries <- c("CL", "CR", "IL", "IS", "KR")
# other_oecd <- c("AU", "CA", "US", "JP", "NO", "NZ", "CH")

oecd_geos_ulcq <- c("AU", "CA", "US", "JP", "NO", "NZ")
oecd_geos <- c("US", "JP")

ameco_extra_geos <- c("AU", "CA", "US", "JP", "NZ", "CH")

all_geos <- c(eurostat_geos, oecd_geos)

all_extra_geos <- c(eu_geo, other_eurostat_geo, c("AU", "CA", "US", "JP", "NO", "NZ", "MX", "CH", "TR", "IL", "KR"))

geo21 <- c(eurostat_geos, ameco_extra_geos)
geo20 <- c(eurostat_geos, c("AU", "CA", "US", "JP", "CH"))

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

usethis::use_data(eurostat_geos, oecd_geos_ulcq, oecd_geos, all_geos,
                  tuku16, geo20,
                  all_extra_geos, ameco_extra_geos,
                  main_nace_sna,
                  main_nace10_sna,
                  a_start_time, q_start_time, base_year, overwrite = TRUE)

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
  lp_ind = "Työn tuottavuus, indeksi",
  d1_per_ind = "Palkansaajakorvaukset, työntekijää kohden, indeksi",
  nulc = "Yksikkötyökustannus",
  nulc_aper = "Yksikkötyökustannus, yrittäjäkorjattu, henkilöä kohden",
  nulc_aper_va = "Yksikkötyökustannus, yrittäjäkorjattu, henkilöä kohden, arvonlisästä",
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

nace_labels_fi <- c(
  TOTAL = "Koko talous",
  manu = "Teollisuus",
  manu_ex26_27 = "Teollisuus pl. 26-27"
)

usethis::use_data(var_labels, var_labels_fi, overwrite = TRUE)
usethis::use_data(nace_labels_fi, overwrite = TRUE)
## Update raw data
