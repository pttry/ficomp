

library(tidyr)
library(dplyr)
library(forcats)

load_all()

q_start_time <- "1995-01-01"
q_base_year <- 2010

## Countries

ea_countries <- eurostat::ea_countries$code
eu_countries<- eurostat::eu_countries$code
other_eurostat_countries <- c("NO", "CH", "IS")
agg_eurostat <- c("EA19", "EU28")

eurostat_geos <- c(eu_countries, other_eurostat_countries, agg_eurostat)

#9 other industrial countries (Australia, Canada, United States, Japan, Norway, New Zealand, Mexico, Switzerland and Turkey)
IC37_other <- c("AU", "CA", "US", "JP", "NO", "NZ", "MX", "CH", "TR")
exec_countries <- c("CL", "CR", "IL", "IS", "KR")
other_oecd <- c("AU", "CA", "US", "JP", "NO", "NZ", "CH")


# Some removed temporary due to missing data
oecd_geos <- setdiff(c("AU", "CA", "US", "JP", "NZ", "CH"), c("CH"))
eurostat_geos <- setdiff(c(eu_countries, other_eurostat_countries), c("CH", "HR", "IS", "PL"))


all_geos <- c(eurostat_geos, oecd_geos)

# setdiff(unique(ulc_eurostat_dat$geo), eurostat::eu_countries$code)
#
# setdiff(eurostat::eu_countries$code, unique(stan_dat$geo))
#
# eurostat::label_eurostat(kk, dic = "geo")
# eurostat::label_eurostat(IC37_other, dic = "geo")

# oecd_geos %in% weights_bis_broad$geo

usethis::use_data(eurostat_geos, oecd_geos)

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
    bkt_ind = rebase(B1GQ__CLV10_MNAC, time = time, baseyear = q_base_year),
    exp_ind = rebase(P6__CLV10_MNAC, time = time, baseyear = q_base_year),
    # tbalance = B11__CP_MEUR,
    nulc_aper = ind_ulc(D1__CP_MNAC / SAL_DC__THS_PER, B1GQ__CLV10_MNAC / EMP_DC__THS_PER, time = time, baseyear = q_base_year)) %>%
  ungroup()

# and from OECD
#  source("data-raw/get_OECD.R")
data("ulc_oecd_dat")

q_dat_oecd_ulc <- ulc_oecd_dat %>%
  filter(time >= q_start_time) %>%
  spread(na_item, values) %>%
  group_by(geo) %>%
  transmute(
    time = time,
    nulc_aper = rebase(NULC_APER, time = time, baseyear = q_base_year)

  ) %>%
  ungroup()

q_dat_oecd <- oecd_dat_Q %>%
  unite(vars, na_item, unit, sep = "__") %>%
  filter(time >= q_start_time) %>%
  spread(vars, values) %>%
  group_by(geo) %>%
  transmute(
    time = time,
    bkt_ind = rebase(B1GQ__CLV_NAC, time = time, baseyear = q_base_year),
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
  mutate(nulc_aper_rel15 = weight_index(nulc_aper, geo, 2015, weight_df = weights_bis_broad)) %>%
  ungroup()


# q_dat %>%
#   group_by(geo) %>%
#   summarise_if(is.numeric, ~sum(is.na(.))) %>%
#   ungroup() %>%
#   filter((bkt_ind + exp_ind + nulc_aper) > 0)

q_dat %>%
  filter(geo == "FI") %>%
  ggplot(aes(time, nulc_aper_rel15)) +
  geom_line()

## Annual data

data("stan_dat")
