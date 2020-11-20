
# Setup

library(tidyr)
library(dplyr)
library(forcats)

devtools::load_all()

# Country groups, dates, labels and other metadata is set in data-raw/data_meta.R

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

q_dat_oecd_large <- oecd_dat_Q %>%
  unite(vars, na_item, unit, sep = "__") %>%
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

q_dat_oecd <- q_dat_oecd_large %>%
  filter(geo %in% oecd_geos)


# Eurostat QNA
# Data from eurostat
#  source("data-raw/eurostat.R")
#  in addition to EU countries also "NO", "CH", "IS"
data("naq_eurostat_dat")

q_dat_eurostat <- naq_eurostat_dat %>%
  unite(vars, na_item, unit, sep = "__") %>%
  filter(nace_r2 == "TOTAL"
         # ,
         # time >= q_start_time
  ) %>%
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
    emp_ind = rebase(EMP_DC__THS_PER, time = time, baseyear = base_year),
    lp_ind = rebase(B1GQ__CLV15_MNAC / EMP_DC__THS_PER, time = time, baseyear = base_year),
    d1_per_ind = rebase(D1__CP_MNAC / SAL_DC__THS_PER, time = time, baseyear = base_year),
    lp_hw_ind = rebase(B1GQ__CLV15_MNAC / EMP_DC__THS_HW, time = time, baseyear = base_year),
    d1_hw_ind = rebase(D1__CP_MNAC / SAL_DC__THS_HW, time = time, baseyear = base_year),
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






## Estimation Q data

calculate_ind <- function(.data, ..., .keep = "all"){
  .data %>%
    group_by(...) %>%
    mutate(
      .keep = .keep,
      geo = geo,
      time = time,
      # Term of trade adujusted GDP
      B1GQA__CLV15_MNAC = rebase(100 * (B1GQ__CLV15_MNAC + (P6__CP_MNAC / (P7__CP_MNAC / P7__CLV15_MNAC)) -
                                          P6__CLV15_MNAC), time = time, baseyear = base_year),
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
    ungroup()
}

calculate_ind_nace <- function(.data, ..., .keep = "all"){
  .data %>%
    group_by(...) %>%
    mutate(
      .keep = .keep,
      geo = geo,
      time = time,
      nulc_va = ind_ulc(D1__CP_MNAC, B1G__CLV15_MNAC, time = time, baseyear = base_year),
      nulc_va_eur = ind_ulc(D1__CP_MEUR, B1G__CLV15_MNAC, time = time, baseyear = base_year),
      nulc_aper_va = ind_ulc(D1__CP_MNAC / SAL_DC__THS_PER, B1G__CLV15_MNAC / EMP_DC__THS_PER, time = time, baseyear = base_year),
      nulc_aper_va_eur = ind_ulc(D1__CP_MEUR / SAL_DC__THS_PER, B1G__CLV15_MNAC / EMP_DC__THS_PER, time = time, baseyear = base_year),
      nulc_hw_va = ind_ulc(D1__CP_MNAC / SAL_DC__THS_HW, B1G__CLV15_MNAC / EMP_DC__THS_HW, time = time, baseyear = base_year),
      nulc_hw_va_eur = ind_ulc(D1__CP_MEUR / SAL_DC__THS_HW, B1G__CLV15_MNAC / EMP_DC__THS_HW, time = time, baseyear = base_year),
      rulc_aper_va = rebase(nulc_aper_va / (B1G__CP_MNAC/B1G__CLV15_MNAC), time = time, baseyear = base_year),
      rulc_hw_va = rebase(nulc_hw_va / (B1G__CP_MNAC/B1G__CLV15_MNAC), time = time, baseyear = base_year),
      emp_ind = rebase(EMP_DC__THS_PER, time = time, baseyear = base_year),
      lp_ind = rebase(B1G__CLV15_MNAC / EMP_DC__THS_PER, time = time, baseyear = base_year),
      d1_per_ind = rebase(D1__CP_MNAC / SAL_DC__THS_PER, time = time, baseyear = base_year),
      va_ind = rebase(B1G__CLV15_MNAC, time = time, baseyear = base_year)) %>%
    ungroup()
}


# OECD ulc data
dat_ulc_oecd_est <- ulc_oecd_dat %>%
  filter(geo %in% c(ameco_extra_geos)) %>%
  spread(na_item, values) %>%
  left_join(select(exh_eur_q, geo, time, exch_eur = values), by = c("geo", "time")) %>%
  group_by(geo) %>%
  transmute(
    time = time,
    nace_r2 = "TOTAL",
    nulc_aper = rebase(NULC_APER, time = time, baseyear = base_year),
    nulc_aper_eur = rebase(NULC_APER/exch_eur, time = time, baseyear = base_year),
    lp_ind = rebase(GDP_EMP_PER, time = time, baseyear = base_year),
    d1_per_ind = rebase(NULC_APER, time = time, baseyear = base_year),
  ) %>%
  ungroup() %>%
  droplevels()



check_ameco <- function(.data){
  y <- .data %>%
    group_by(geo) %>%
    summarise(across(!all_of(c("time")), ~all(is.na(.x))), .groups = "drop_last") %>%
    ungroup() %>%
    gather(vars, values, -geo) %>%
    filter(values)

  if(nrow(y)>0) print()
  .data
}

# Use splines to make quarterly data from annual AMECO data
dat_ameco_q_est <-
  data_ameco %>%
  filter(geo %in% c(eurostat_geos, ameco_extra_geos)) %>%
  calculate_ind(geo, .keep = "none") %>%
  select(-B1GQA__CLV15_MNAC) %>%
  mutate(time = lubridate::ymd(paste0(time, "-07-01"))) %>%
  right_join(tibble(time = seq.Date(as.Date("1991-01-01"), as.Date("2019-10-01"), by = "quarter")),
             by = "time") %>%
  droplevels() %>%
  complete(time, geo) %>%
  filter(!is.na(geo)) %>%
  # Delete variables with missing data, check_ameco should print empty tibble
  select(-nulc_va, -nulc_aper_va) %>%
  check_ameco() %>%
  # Estimate quarterly based on spline
  group_by(geo) %>%
  mutate(across(where(is.numeric), ~stats::spline(time, .x, xout = time)$y)) %>%
  mutate(across(where(is.numeric), ~rebase(.x, time, base_year))) %>%
  ungroup()


data_quartely_est <-
  q_dat_eurostat %>%
  filter(geo %in% eurostat_geos) %>%
  filter(nace_r2 == "TOTAL") %>%
  # # join nulc_aper from OECD ulc and replace nulc if missing (in mutate)
  # left_join(select(q_dat_oecd_ulc, geo, time, nulc_aper_oecd = nulc_aper),
  #           by = c("geo", "time")) %>%
  droplevels() %>%
  complete(geo, time) %>%
  calculate_ind(geo) %>%
  # Add OECD for more countries with some data
  bind_rows(dat_ulc_oecd_est) %>%
  select(-nace_r2) %>%
  filter(time >= "1991-01-01") %>%
  complete(geo, time) %>%
  # Replace NAs with quarterlylized annual data from ameco
  left_join(dat_ameco_q_est, by = c("geo", "time"), suffix = c("", "_long")) %>%
  mutate(across(ends_with("_long"), ~coalesce(cur_data()[[gsub("_long", "", cur_column())]], .x))) %>%
  # Exchange rate, from data
  mutate(exch_eur = nulc_long / nulc_eur_long) %>%
  # Weight all
  group_by(time) %>%
  mutate(across(-c("geo", matches("^[A-Z]", ignore.case = FALSE)),
                ~weight_index2(.x, geo, time, geos = eurostat_geos, weight_df = weights_ecfin37),
                .names = paste0("{col}_rel_ecfin15"))) %>%
  mutate(across(-c("geo", matches("^[A-Z]", ignore.case = FALSE), contains("_rel_")),
                ~weight_index2(.x, geo, time, geos = geo20, weight_df = weights_ecfin37),
                .names = paste0("{col}_rel_ecfin20"))) %>%
  mutate(across(nulc_aper_eur_long,
                ~weight_index2(.x, geo, time, geos = geo14, weight_df = weights_ecfin37),
                .names = paste0("{col}_rel_ecfin14"))) %>%
  ungroup() %>%
  # Extra data from Economic Outlook
  left_join(select(filter(eo_q_dat),
                   geo, time, XPERF, XGSVD, XMKT, eci), by = c("geo", "time")) %>%
  mutate(geo = as_factor(geo))


usethis::use_data(data_quartely_est, data_quartely_est, overwrite = TRUE)
write.csv2(data_quartely_est, file = "data-out/data_quartely_est.csv")
saveRDS(data_quartely_est, file = "data-out/data_quartely_est.rds")



## Quarterly industry group data

data_eurostat_nace_q <- naq_eurostat_nace_dat %>%
  unite(vars, na_item, unit, sep = "__") %>%
  filter(time >= q_start_time) %>%
  spread(vars, values)

# visdat::vis_dat(data_eurostat_nace_q)


data_main_nace_q <-
  data_eurostat_nace_q %>%
  # filter(geo != "BE") %>%
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
  calculate_ind_nace(geo, nace0) %>%
  group_by(nace0, time) %>%
  mutate(across(-c("geo", matches("^[A-Z]", ignore.case = FALSE)),
                ~weight_index(.x, geo, time, weight_df = weights_ecfin27, check_geos = FALSE),
                .names = paste0("{col}_rel_ecfin13"))) %>%
  ungroup()





usethis::use_data(data_main_groups_q, overwrite = TRUE)
write.csv2(data_main_groups_q, file = "data-out/data_main_groups_quarterly.csv")
saveRDS(data_main_groups_q, file = "data-out/data_main_groups_quarterly.rds")


## Quarterly series extended with annual data

# Use splines to make quarterly data from annual ILC data
dat_ilc_q_est <-
  data_ilc %>%
  mutate(time = lubridate::ymd(paste0(time, "-07-01"))) %>%
  filter(geo %in% c(eurostat_geos, ameco_extra_geos)) %>%
  filter(nace0 %in% "manu") %>%
  # only non relative indeces
  select(c(!matches("_rel_") & matches("[a-z]", ignore.case = FALSE))) %>%
  select(-cpi10, -nac_usd) %>%
  droplevels() %>%
  # add quarters
  right_join(
    expand.grid(time = seq.Date(min(.$time) - months(6),
                                max(.$time) + months(3),
                                by = "quarter"),
                geo = unique(.$geo),
                nace0 = unique(.$nace0)),
    by = c("geo", "time", "nace0") ) %>%
  complete(time, geo, nace0) %>%
  filter(!is.na(geo), !is.na(nace0)) %>%
  # Estimate quarterly based on spline
  group_by(geo, nace0) %>%
  mutate(across(where(is.numeric), ~stats::spline(time, .x, xout = time)$y)) %>%
  mutate(across(where(is.numeric), ~rebase(.x, time, base_year))) %>%
  ungroup()



data_quartely_manu_est <-
  data_main_groups_q %>%
  filter(geo %in% c(eurostat_geos, ameco_extra_geos)) %>%
  filter(nace0 %in% "manu") %>%
  select(!matches("_rel_")) %>%
  droplevels() %>%
  right_join(
    expand.grid(
      time = seq.Date(
        min(dat_ilc_q_est$time),
        max(.$time),
        by = "quarter"),
      geo = unique(dat_ilc_q_est$geo),
      nace0 = unique(.$nace0)),
    by = c("geo", "time", "nace0") ) %>%
  complete(geo, time, nace0) %>%
  # Replace NAs with quarterlylized annual data from ameco
  left_join(filter(dat_ilc_q_est, nace0 == "manu"),
            by = c("geo", "time", "nace0"), suffix = c("", "_long")) %>%
  mutate(across(ends_with("_long"), ~coalesce(cur_data()[[gsub("_long", "", cur_column())]], .x))) %>%
  # Weight all
  group_by(time, nace0) %>%
  mutate(across(-c("geo", matches("^[A-Z]", ignore.case = FALSE)),
                ~weight_index2(.x, geo, time, geos = eurostat_geos, weight_df = weights_ecfin37),
                .names = paste0("{col}_rel_ecfin15"))) %>%
  mutate(across(-c("geo", matches("^[A-Z]", ignore.case = FALSE), contains("_rel_")),
                ~weight_index2(.x, geo, time, geos = geo20, weight_df = weights_ecfin37),
                .names = paste0("{col}_rel_ecfin20"))) %>%
  ungroup() %>%
  mutate(geo = as_factor(geo))



usethis::use_data(data_quartely_manu_est, overwrite = TRUE)
write.csv2(data_quartely_manu_est, file = "data-out/data_quartely_manu_est.csv")
saveRDS(data_quartely_manu_est, file = "data-out/data_quartely_manu_est.rds")
