

load_all()

q_start_time <- "1995-01-01"
q_base_year <- 2010

## Countries

ea_countries <- eurostat::ea_countries$code
eu_countries<- eurostat::eu_countries$code
other_eurostat_countries <- c("NO", "CH", "IS")
agg_eurostat <- c("EA19", "EU28")

eurostat_geos <- c(eu_countries, other_eurostat, agg_eurostat)

#9 other industrial countries (Australia, Canada, United States, Japan, Norway, New Zealand, Mexico, Switzerland and Turkey)
IC37_other <- c("AU", "CA", "US", "JP", "NO", "NZ", "MX", "CH", "TR")
exec_countries <- c("CL", "CR", "IL", "IS", "KR")
other_oecd <- c("AU", "CA", "US", "JP", "NO", "NZ", "CH")


oecd_geos <- c("AU", "CA", "US", "JP", "NZ", "CH")

# setdiff(unique(ulc_eurostat_dat$geo), eurostat::eu_countries$code)
#
# setdiff(eurostat::eu_countries$code, unique(stan_dat$geo))
#
# eurostat::label_eurostat(kk, dic = "geo")
# eurostat::label_eurostat(IC37_other, dic = "geo")


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
  bind_rows(q_dat_oecd)


## Annual data

data("stan_dat")
