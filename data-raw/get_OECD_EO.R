# OECD Economic Outlook database

library(OECD)
library(tidyverse)
library(countrycode)

devtools::load_all()

# dataset_list <- get_datasets()
# search_dataset("Outlook", data = dataset_list)  %>% View()


eo_str <- get_data_structure("EO")

eo_str$VARIABLE %>% knitr::kable()
# eo$IND %>% knitr::kable()
# eo_str$LOCATION %>% knitr::kable()


# Needed countries that are in STAN
loc_list <- intersect(
  countrycode(c(eurostat_geos, oecd_geos, c("AU", "CA", "US", "JP", "NZ", "CH")), "eurostat", "iso3c"),
  stan_str$LOCATION$id)


var_list_exp <- c("SHTGSVD", "MSHA", "XSHA", "CTGSVD", "MPEN", "XMKT", "XPERF", "TGSVD")

var_list_ulc <- c("HRS", "PDTY", "ULC", "WRT", "WSST")

eo_str$VARIABLE %>%
  filter(id %in% var_list) %>%
  knitr::kable()

dat_eo_exp0 <- get_dataset("EO", filter = list(loc_list, var_list_exp))
dat_eo_ulc0 <- get_dataset("EO", filter = list(loc_list, var_list_ulc))
# dat_eo0_fi <- get_dataset("EO", filter = list("FIN", var_list))


# Quarterly data
eo_q_dat <-
  dat_eo_exp0 %>%
  filter(FREQUENCY == "Q") %>%
  transmute(geo = as_factor(countrycode(LOCATION, "iso3c", "eurostat")),
            vars = as_factor(VARIABLE),
            freq = as_factor(FREQUENCY),
            time = lubridate::yq(obsTime),
            values = obsValue)

# Annual data
eo_a_dat <-
  dat_eo_exp0 %>%
  filter(FREQUENCY == "A") %>%
  transmute(geo = as_factor(countrycode(LOCATION, "iso3c", "eurostat")),
            vars = as_factor(VARIABLE),
            freq = as_factor(FREQUENCY),
            time = as.numeric(obsTime),
            values = obsValue)

eo_q_dat %>%
  ggplot(aes(time, values, group = geo)) +
  geom_line() +
  facet_wrap(~ vars, scales = "free_y")

eo_a_dat %>%
  ggplot(aes(time, values, group = geo)) +
  geom_line() +
  facet_wrap(~ vars, scales = "free_y")


# Quarterly data
eo_ulc_q_dat <-
  dat_eo_ulc0 %>%
  filter(FREQUENCY == "Q") %>%
  transmute(geo = as_factor(countrycode(LOCATION, "iso3c", "eurostat")),
            vars = as_factor(VARIABLE),
            time = lubridate::yq(obsTime),
            values = obsValue)

# Annual data
eo_ulc_a_dat <-
  dat_eo_ulc0 %>%
  filter(FREQUENCY == "A") %>%
  transmute(geo = as_factor(countrycode(LOCATION, "iso3c", "eurostat")),
            vars = as_factor(VARIABLE),
            time = as.numeric(obsTime),
            values = obsValue)

eo_ulc_q_dat %>%
  ggplot(aes(time, values, group = geo)) +
  geom_line() +
  facet_wrap(~ vars, scales = "free_y")

eo_ulc_a_dat %>%
  ggplot(aes(time, values, group = geo)) +
  geom_line() +
  facet_wrap(~ vars, scales = "free_y")

# %>%
  # spread(vars, values) %>%
  # mutate(EMP_DC__THS_HW = EMP_DC__MIL_HW * 1000,
  #        SAL_DC__THS_HW = SAL_DC__MIL_HW * 1000) %>%
  # select(- EMP_DC__MIL_HW, - SAL_DC__MIL_HW) %>%
  # group_by(geo, nace_r2) %>%
  # mutate(B1G__PYP_MNAC = statfitools::pp(cp = B1G__CP_MNAC, fp = B1G__CLV10_MNAC, time = time)) %>%
  # ungroup()

# TODO: open sector

eo_dat %>%
  filter(freq == "Q") %>%
  droplevels() %>%
  str()


usethis::use_data(oecd_eo_dat, overwrite = TRUE)

