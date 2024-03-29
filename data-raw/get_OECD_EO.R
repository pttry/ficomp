# OECD Economic Outlook database

library(OECD)
library(tidyverse)
library(countrycode)

devtools::load_all()

# dataset_list <- get_datasets()
# search_dataset("Outlook", data = dataset_list)  %>% View()

start_eo <- "1991-01-01"

eo_str <- get_data_structure("EO")

# eo_str$VARIABLE %>% knitr::kable()
# eo$IND %>% knitr::kable()
# eo_str$LOCATION %>% knitr::kable()


# Needed countries that are in STAN, missing for Q are c("NO", "DK", "NZ", "ES")
loc_list <- intersect(
  countrycode(c(eurostat_geos, oecd_geos), "eurostat", "iso3c"),
  eo_str$LOCATION$id)

loc_list_large <- intersect(
  countrycode(c(eurostat_geos, oecd_geos_ulcq), "eurostat", "iso3c"),
  eo_str$LOCATION$id)

# var_list_exp <- c("SHTGSVD", "MSHA", "XSHA", "CTGSVD", "MPEN", "XMKT", "XPERF", "TGSVD")
#
# var_list_ulc <- c(nulc = "ULC",
#                   D1__CP_MNAC = "WSSS",
#                   B11__CP_MNAC = "FBGS",
#                   B1GQ__CP_MNAC = "GDP",
#                   B1GQ__CLV10_MNAC = "GDPV",
#                   P6__CLV10_MNAC = "XGSV",
#                   P6__CP_MNAC = "XGS")

var_list_eo <- c(nulc = "ULC",
                 D1__CP_MNAC = "WSSS",
                 B11__CP_MNAC = "FBGS",
                 B1GQ__CP_MNAC = "GDP",
                 B1GQ__CLV10_MNAC = "GDPV",
                 P6__CLV10_MNAC = "XGSV",
                 P6__CP_MNAC = "XGS",
                 XPERF = "XPERF",
                 XSHA = "XSHA",
                 XGSVD = "XGSVD",
                 XMKT = "XMKT",
                 WRT = "WRT",
                 D1_aper__CP_MNAC  = "WSST",
                 EE = "EE"
                 )


# eo_str$VARIABLE %>%
#   filter(id %in% var_list_exp) %>%
#   knitr::kable()


dat_eo_0 <- get_dataset("EO", filter = list(loc_list, var_list_eo))


dat_eo_0_large <- get_dataset("EO", filter = list(loc_list_large, var_list_eo))


# Quarterly data
 # note no XSHA in q-data

# eo_q_dat_large <-
  # dat_eo_0_large %>%

eo_q_dat <-
  dat_eo_0 %>%
  filter(FREQUENCY == "Q") %>%
  transmute(geo = as_factor(countrycode(LOCATION, "iso3c", "eurostat")),
            vars = fct_recode(VARIABLE, !!!var_list_eo[!(var_list_eo %in% "XSHA")]),
            unit = as_factor(UNIT),
            time = lubridate::yq(Time),
            values = as.numeric(ObsValue)) %>%
  filter(time >= start_eo,
         !(geo %in% c("NO", "DK", "NZ", "ES"))) %>%
  droplevels() %>%
  select(-unit) %>%
  complete(geo, time, vars) %>%
  spread(vars, values) %>%
  group_by(geo) %>%
  mutate(
    nulc = rebase(nulc, time = time, baseyear = a_base_year),
    gdp_ind = rebase(B1GQ__CLV10_MNAC, time = time, baseyear = a_base_year),
    exp_ind = rebase(P6__CLV10_MNAC, time = time, baseyear = a_base_year),
    tbalance_gdp = B11__CP_MNAC / B1GQ__CP_MNAC) %>%
  ungroup() %>%
  mutate(year = lubridate::year(time)) %>%
  group_by(time) %>%
  mutate(nulc_rel15 = weight_index(nulc, geo, 2015, weight_df = weights_bis_broad),
         nulc_rel = weight_index(nulc, geo, year, weight_df = weights_bis_broad),
         nulc_rel_imf = weight_index(nulc, geo, year, weight_df = weights_imf),
         gdp_ind_rel15 = weight_index(gdp_ind, geo, 2015, weight_df = weights_bis_broad),
         exp_ind_rel15 = weight_index(exp_ind, geo, 2015, weight_df = weights_bis_broad)) %>%
  ungroup() %>%
  left_join(select(eci_dat, geo, year = time, eci), by = c("geo", "year"))

eo_q_dat_large <-
  dat_eo_0_large %>%
  filter(FREQUENCY == "Q") %>%
  transmute(geo = as_factor(countrycode(LOCATION, "iso3c", "eurostat")),
            vars = fct_recode(VARIABLE, !!!var_list_eo[!(var_list_eo %in% "XSHA")]),
            unit = as_factor(UNIT),
            time = lubridate::yq(Time),
            values = as.numeric(ObsValue)) %>%
  # filter(year > 1990, year < 2019,
  #        !(geo %in% c("DK", "CH", "ES"))) %>%
  droplevels() %>%
  select(-unit) %>%
  complete(geo, time, vars) %>%
  spread(vars, values) %>%
  group_by(geo) %>%
  mutate(
    nulc = rebase(nulc, time = time, baseyear = a_base_year),
    gdp_ind = rebase(B1GQ__CLV10_MNAC, time = time, baseyear = a_base_year),
    exp_ind = rebase(P6__CLV10_MNAC, time = time, baseyear = a_base_year),
    tbalance_gdp = B11__CP_MNAC / B1GQ__CP_MNAC) %>%
  ungroup()


# Annual data
eo_a_dat <-
  dat_eo_0 %>%
  filter(FREQUENCY == "A") %>%
  transmute(geo = as_factor(countrycode(LOCATION, "iso3c", "eurostat")),
            vars = fct_recode(VARIABLE, !!!var_list_eo),
            unit = as_factor(UNIT),
            time = as.numeric(Time),
            values = as.numeric(ObsValue)) %>%
  filter(time >= lubridate::year(start_eo)) %>%
  select(-unit) %>%
  complete(geo, time, vars) %>%
  spread(vars, values) %>%
  group_by(geo) %>%
  mutate(
    nulc = rebase(nulc, time = time, baseyear = a_base_year),
    gdp_ind = rebase(B1GQ__CLV10_MNAC, time = time, baseyear = a_base_year),
    exp_ind = rebase(P6__CLV10_MNAC, time = time, baseyear = a_base_year),
    tbalance_gdp = B11__CP_MNAC / B1GQ__CP_MNAC) %>%
  ungroup() %>%
  group_by(time) %>%
  mutate(nulc_rel15 = weight_index(nulc, geo, 2015, weight_df = weights_bis_broad),
         nulc_rel = weight_index(nulc, geo, time, weight_df = weights_bis_broad),
         nulc_rel_imf = weight_index(nulc, geo, time, weight_df = weights_imf),
         gdp_ind_rel15 = weight_index(gdp_ind, geo, 2015, weight_df = weights_bis_broad),
         exp_ind_rel15 = weight_index(exp_ind, geo, 2015, weight_df = weights_bis_broad)) %>%
  ungroup() %>%
  left_join(select(eci_dat, geo, time, eci), by = c("geo", "time"))



# visdat::vis_dat(eo_q_dat)


# %>%
  # spread(vars, values) %>%
  # mutate(EMP_DC__THS_HW = EMP_DC__MIL_HW * 1000,
  #        SAL_DC__THS_HW = SAL_DC__MIL_HW * 1000) %>%
  # select(- EMP_DC__MIL_HW, - SAL_DC__MIL_HW) %>%
  # group_by(geo, nace_r2) %>%
  # mutate(B1G__PYP_MNAC = statfitools::pp(cp = B1G__CP_MNAC, fp = B1G__CLV10_MNAC, time = time)) %>%
  # ungroup()

# TODO: open sector

# eo_dat %>%
#   filter(freq == "Q") %>%
#   droplevels() %>%
#   str()


usethis::use_data(eo_a_dat, eo_q_dat, eo_q_dat_large, overwrite = TRUE)


write.csv2(eo_a_dat, file = "data-out/eo_annual_data.csv")
saveRDS(eo_a_dat, file = "data-out/eo_annual_data.rds")

write.csv2(eo_q_dat, file = "data-out/eo_quarterly_data.csv")
saveRDS(eo_q_dat, file = "data-out/eo_quarterly_data.rds")
