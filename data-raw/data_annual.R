
# Setup

library(tidyr)
library(dplyr)
library(forcats)

devtools::load_all()

source("data-raw/data_meta.R")

# Country groups, dates, labels and other metadata is set in data-raw/data_meta.R


## Final annual data

data("data_eurostat_nama_nace_a", "data_oecd_sna_nace_a", "data_ameco")

# Compine Eurostat and OECD annual data

data_main_nace_a <-
  data_eurostat_nama_nace_a %>%
  bind_rows(select(data_oecd_sna_nace_a, all_of(names(.)))) %>%
  filter(time >= a_start_time,
         geo %in% all_geos) %>%
  mutate(geo = as_factor(geo))


# visdat::vis_dat(data_main_nace_a)

# Value added based data with industry groups

data_main_groups_a <-
  data_main_nace_a %>%
  gather(vars, values, -time, - geo, - nace_r2) %>%
  group_by(geo, time, vars) %>%
  summarise(
    total = values[nace_r2 == "TOTAL"],
            private = sum(values[nace_r2 %in% c("C", "G", "H", "I", "J", "M", "N")]),
            private_ex26 = private - values[nace_r2 == "C26"],
            manu = sum(values[nace_r2 == "C"]),
            manu_ex26 = manu - values[nace_r2 == "C26"],
            service = sum(values[nace_r2 %in% c("G", "H", "I", "J", "M", "N")])) %>%
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
    va_ind = rebase(B1G__CLV15_MNAC, time = time, baseyear = base_year),
    emp_ind = rebase(EMP_DC__THS_HW, time = time, baseyear = base_year),
    lp_hw_va = rebase(B1G__CLV15_MNAC / EMP_DC__THS_HW, time = time, baseyear = base_year),
    d1_hw = rebase(D1__CP_MNAC / SAL_DC__THS_HW, time = time, baseyear = base_year),
    nulc_va = ind_ulc(D1__CP_MNAC, B1G__CLV15_MNAC, time = time, baseyear = base_year),
    nulc_va_eur = ind_ulc(D1__CP_MEUR, B1G__CLV15_MNAC, time = time, baseyear = base_year),
    nulc_aper_va = ind_ulc(D1__CP_MNAC / SAL_DC__THS_PER, B1G__CLV15_MNAC / EMP_DC__THS_PER, time = time, baseyear = base_year),
    nulc_hw_va = ind_ulc(D1__CP_MNAC / SAL_DC__THS_HW, B1G__CLV15_MNAC / EMP_DC__THS_HW, time = time, baseyear = base_year),
    nulc_hw_va_eur = ind_ulc(D1__CP_MEUR / SAL_DC__THS_HW, B1G__CLV15_MNAC / EMP_DC__THS_HW, time = time, baseyear = base_year),
    rulc_hw_va = rebase(nulc_hw_va / (B1G__CP_MNAC/B1G__CLV15_MNAC), time = time, baseyear = base_year)) %>%
  group_by(nace0) %>%
  weight_all(geo, time, except = c("geo", "time"), weight_df = weights_ecfin37) %>%
  ungroup()


data("data_eurostat_nama_a")

data_main_total_a <-
  data_eurostat_nama_a %>%
  # Following are missing from OECD
  select( -B1G__CLV15_MNAC, -B1G__CP_MNAC, -B1G__CP_MEUR, -B1G__PYP_MNAC,
          -EMP_DC__THS_HW, -SAL_DC__THS_HW,
          -EMP_DC__THS_PER, -SAL_DC__THS_PER,
          -P7__CLV15_MNAC, -P7__CP_MEUR, -P7__CP_MNAC) %>%
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
  group_by(geo) %>%
  mutate(
    # Term of trade adujusted GDP
    B1GQA__CLV15_MNAC = 100 * (B1GQ__CLV15_MNAC + (P6__CP_MNAC / (P7__CP_MNAC / P7__CLV15_MNAC)) -
                     P6__CLV15_MNAC) / B1GQ__CLV15_MNAC[time == 2015],
    nulc = ind_ulc(D1__CP_MNAC, B1GQ__CLV15_MNAC, time = time, baseyear = base_year),
    nulc_eur = ind_ulc(D1__CP_MEUR, B1GQ__CLV15_MNAC, time = time, baseyear = base_year),
    nulc_va = ind_ulc(D1__CP_MNAC, B1G__CLV15_MNAC, time = time, baseyear = base_year),
    nulc_aper = ind_ulc(D1__CP_MNAC / SAL_DC__THS_PER, B1GQ__CLV15_MNAC / EMP_DC__THS_PER, time = time, baseyear = base_year),
    nulc_aper_atot = ind_ulc(D1__CP_MNAC / SAL_DC__THS_PER, B1GQA__CLV15_MNAC / EMP_DC__THS_PER, time = time, baseyear = base_year),
    nulc_aper_eur_atot = ind_ulc(D1__CP_MEUR / SAL_DC__THS_PER, B1GQA__CLV15_MNAC / EMP_DC__THS_PER, time = time, baseyear = base_year),
    nulc_aper_eur = ind_ulc(D1__CP_MEUR / SAL_DC__THS_PER, B1GQ__CLV15_MNAC / EMP_DC__THS_PER, time = time, baseyear = base_year),
    nulc_aper_va = ind_ulc(D1__CP_MNAC / SAL_DC__THS_PER, B1G__CLV15_MNAC / EMP_DC__THS_PER, time = time, baseyear = base_year),
    rulc_aper = rebase(nulc_aper / (B1GQ__CP_MNAC/B1GQ__CLV15_MNAC), time = time, baseyear = base_year),
    emp_ind = rebase(EMP_DC__THS_PER, time = time, baseyear = base_year),
    lp_ind = rebase(B1GQ__CLV15_MNAC / EMP_DC__THS_PER, time = time, baseyear = base_year),
    d1_per_ind = rebase(D1__CP_MNAC / SAL_DC__THS_PER, time = time, baseyear = base_year),
    gdp_ind = rebase(B1GQ__CLV15_MNAC, time = time, baseyear = base_year),
    exp_ind = rebase(P6__CLV15_MNAC, time = time, baseyear = base_year),
    exp_goods_ind = rebase(P61__CLV15_MNAC, time = time, baseyear = base_year),
    exp_serv_ind = rebase(P62__CLV15_MNAC, time = time, baseyear = base_year),
    tbalance_gdp = B11__CP_MNAC / B1GQ__CP_MNAC) %>%
  # Weight all
  group_by(time) %>%
  mutate(across(-c("geo", matches("^[A-Z]", ignore.case = FALSE)),
                ~weight_index2(.x, geo, time, geos = eurostat_geos, weight_df = weights_ecfin37),
                .names = paste0("{col}_rel_ecfin15"))) %>%
  mutate(across(-c("geo", matches("^[A-Z]", ignore.case = FALSE), contains("_rel_")),
                ~weight_index2(.x, geo, time, geos = geo20, weight_df = weights_ecfin37),
                .names = paste0("{col}_rel_ecfin20"))) %>%
  ungroup() %>%
  left_join(select(filter(eo_a_dat, time >= a_start_time),
                   geo, time, XPERF, XSHA, XGSVD, XMKT, eci), by = c("geo", "time")) %>%
  mutate(geo = as_factor(geo))




usethis::use_data(data_main_groups_a, data_main_total_a, data_main_annual, overwrite = TRUE)
write.csv2(data_main_groups_a, file = "data-out/data_main_groups_annual.csv")
saveRDS(data_main_groups_a, file = "data-out/data_main_groups_annual.rds")

write.csv2(data_main_total_a, file = "data-out/data_main_total_annual.csv")
saveRDS(data_main_total_a, file = "data-out/data_main_total_annual.rds")

write.csv2(data_main_annual, file = "data-out/data_main_annual.csv")
saveRDS(data_main_annual, file = "data-out/data_main_annual.rds")

