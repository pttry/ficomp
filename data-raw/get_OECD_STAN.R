# OECD data

library(OECD)
library(tidyverse)
library(countrycode)

# dataset_list <- get_datasets()
# search_dataset("STAN", data = dataset_list) # %>% View()


stan_str <- get_data_structure("STANI4_2016")

# stan_str$VAR %>% knitr::kable()
# stan_str$IND %>% knitr::kable()
# stan_str$LOCATION %>% knitr::kable()


ind_codes <- c(D01T99 = "TOTAL", D10T33 = "C", D41T43 = "F", D45T47 = "G", D49T53 = "H",
               D55T56 = "I", D58T63 = "J", D69T75 = "M", D77T82 = "N")


stan_str$LOCATION %>%
  mutate(code = countrycode(stan_str$LOCATION$id, "iso3c", "eurostat")) %>%
  full_join(eurostat::eu_countries, by = c("code"))


ind_list <- setNames(names(ind_codes), ind_codes)


# Huom!
var_list <- c(B1G__CP_MNAC = "VALU", B1G__CLV10_MNAC = "VALK", B1G__PYP_MNAC = "VKPY",
              EMP_DC__THS_PER = "EMPN", EMP_DC__MIL_HW = "HRSN", SAL_DC__THS_HW = "HRSE", D1__CP_MNAC = "LABR")


dat_stan0 <- get_dataset("STANI4_2016", filter = list(stan_str$LOCATION$id, var_list, ind_list))

# dat_stan %>%
#   select(VAR, UNIT, LOCATION) %>%
#   distinct() %>% View()

stan_dat <-
  dat_stan0 %>%
  transmute(geo = countrycode(LOCATION, "iso3c", "eurostat"),
            vars = fct_recode(VAR, !!!var_list),
            nace_r2 = fct_recode(IND, !!!ind_list),
            time = as.numeric(obsTime),
            values = obsValue) %>%
  spread(vars, values) %>%
  mutate(EMP_DC__THS_HW = EMP_DC__MIL_HW * 1000) %>%
  select(- EMP_DC__MIL_HW) %>%
  group_by(geo, nace_r2) %>%
  mutate(B1G__PYP_MNAC = statfitools::pp(cp = B1G__CP_MNAC, fp = B1G__CLV10_MNAC, time = time)) %>%
  ungroup()


usethis::use_data(stan_dat, overwrite = TRUE)

