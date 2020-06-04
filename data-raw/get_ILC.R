# Get Conference Board International Comparisons of Manufacturing Productivity and Unit Labor Costs - Data
# https://www.conference-board.org/ilcprogram/index.cfm?id=30139
# Adjusted flat file downloaded

library(dplyr)
library(tidyr)
library(forcats)
library(readr)
library(here)

devtools::load_all()

ilc_flat0 <-
  readr::read_tsv(
    here("data-raw/ILCProductivityULCAllDataDec20191.txt"),
    skip = 2,
    col_types = cols(
      COUNTRY = col_factor(),
      INDUSTRY = col_factor(),
      YEAR = col_integer(),
      VaN = col_number(),
      VaR = col_number(),
      EmA = col_number(),
      HrA = col_number(),
      CmA = col_number(),
      XR = col_double(),
      CPI = col_double()
    )
  )

col_names0 = c(
  COUNTRY = "geo",
  INDUSTRY = "nace",
  YEAR = "time",
  VaN = "B1G__CP_MNAC",
  VaR = "B1G__CLV15_MNAC",
  EmA = "EMP_DC__THS_PER",
  HrA = "EMP_DC__THS_HW",
  CmA = "D1__CP_MNAC",
  XR = "nac_usd",
  CPI = "cpi10"
)

col_names <- setNames(names(col_names0), col_names0)


# There are errors in data deliminators, they are removed
dat_ilc <- ilc_flat0 %>%
  rename(!!!col_names) %>%
  filter(!is.na(geo)) %>%
  mutate(geo = countrycode::countrycode(geo, "iso3c", "eurostat",
                                        custom_match = c(ROM = "RO"))) %>%
  mutate_at(vars(matches("CP_MNAC")),
            .funs = list(EUR = ~EUR(.*nac_usd, time, "USD", exh_eur_a))) %>%
  rename_at(vars(contains("MNAC_EUR")), list(~gsub("NAC_", "", .)))

data_ilc <- dat_ilc %>%
  filter(geo %in% tuku15,
         nace %in% c("C", "26-27"),
         time >= 1990) %>%
  group_by(geo, nace) %>%
  mutate(B1G__PYP_MNAC = statfitools::pp(cp = B1G__CP_MNAC, fp = B1G__CLV15_MNAC, time = time)) %>%
  ungroup() %>%
  gather(vars, values, -time, - geo, - nace) %>%
  group_by(geo, time, vars) %>%
  summarise(
    manu = sum(values[nace == "C"]),
    manu_ex26_27 = manu - values[nace == "26-27"]) %>%
  ungroup() %>%
  gather(nace0, values, manu, manu_ex26_27) %>%
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
    nulc_va = ind_ulc(D1__CP_MNAC, B1G__CLV15_MNAC, time = time, baseyear = base_year),
    nulc_va_eur = ind_ulc(D1__CP_MEUR, B1G__CLV15_MNAC, time = time, baseyear = base_year),
    rulc_va = rebase(nulc_va / (B1G__CP_MNAC/B1G__CLV15_MNAC), time = time, baseyear = base_year)) %>%
  # group_by(time, nace0) %>%
  # mutate(
  #   nulc_va_rel = weight_index(nulc_va, geo, time, weight_df = weights_bis_broad)) %>%
  group_by(nace0) %>%
  weight_at(geo, time, at = c("nulc_va", "nulc_va_eur"), weight_df = weights_ecfin42) %>%
  group_by(nace0) %>%
  weight_at(geo, time, at = c("nulc_va", "nulc_va_eur"), weight_df = weights_bis_narrow)



usethis::use_data(data_ilc, overwrite = TRUE)




# old

# ilc_flat0 <-
#   readr::read_tsv(
#     here("data-raw/ILCProductivityULCAllDataJul2018.txt"),
#     col_types = cols(
#       COUNTRY = col_factor(),
#       INDUSTRY = col_factor(),
#       YEAR = col_integer(),
#       `Nominal Value Added` = col_number(),
#       `Real Value Added` = col_number(),
#       Employment = col_number(),
#       `Total Hours` = col_number(),
#       `Total Labor Cost` = col_number(),
#       `National Currency per US dollar` = col_double(),
#       `Consumer Price Index (2010=100)` = col_double(),
#       X11 = col_skip()
#     )
#   )
#
# col_names0 = c(
#   COUNTRY = "country",
#   INDUSTRY = "industry",
#   YEAR = "year",
#   `Nominal Value Added` = "va_cp",
#   `Real Value Added` = "va_fp",
#   Employment = "emp",
#   `Total Hours` = "hws",
#   `Total Labor Cost` = "lc",
#   `National Currency per US dollar` = "nac_usd",
#   `Consumer Price Index (2010=100)` = "cpi10"
# )
#
# col_names <- setNames(names(col_names0), col_names0)
#
#
# # There are errors in data deliminators, they are removed
# ilc_dat <- ilc_flat0[-problems(ilc_flat0)$row,] %>%
#   rename(!!!col_names)
