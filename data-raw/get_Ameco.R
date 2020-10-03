# Get AMECO data
# https://ec.europa.eu/info/business-economy-euro/indicators-statistics/economic-databases/macro-economic-database-ameco/download-annual-data-set-macro-economic-database-ameco_en

# AMECO - Gross Domestic Product (Income Approach), Labour Costs

library(readr)
library(dplyr)
library(tidyr)
library(forcats)

## Read Ameco tables from https://ec.europa.eu/info/business-economy-euro/indicators-statistics/economic-databases/macro-economic-database-ameco/download-annual-data-set-macro-economic-database-ameco_en

# warnings because ameco files are mal formated.

# POPULATION AND EMPLOYMENT
ameco1_0 <- read_ameco(table_num = 1)
# Domestic Product
ameco6_0 <- read_ameco(table_num = 6)
# Gross Domestic Product (Income Approach), Labour Costs
ameco7_0 <- read_ameco(table_num = 7)
# Exports and Imports of Goods And Services, National Accounts
ameco9_0 <- read_ameco(table_num = 9)
#  BALANCES WITH THE REST OF THE WORLD
ameco10_0 <- read_ameco(table_num = 10)


# Table 1

ameco1_vars <- c(
  "NETD", "NWTD"
)

ameco1_vars_trans <- c(
  EMP_DC__THS_PER = "NETD_0_0_0",
  SAL_DC__THS_PER = "NWTD_0_0_0"
)

ameco1 <- ameco1_0 %>%
  separate(code, into = c("geo", NA, "desc", "unit_code", "rel", "vars"), sep = "\\.", remove = FALSE) %>%
  filter(vars %in% ameco1_vars) %>%
  filter(desc %in% c(0,1)) %>%  # Only current values weights, 2 is PPS weights (it seems)
  unite(vars, vars, unit_code, desc, rel, sep = "_") %>%
  mutate_if(is.character, as_factor) %>%
  droplevels()%>%
  mutate(vars = fct_recode(vars, !!!ameco1_vars_trans))


# Table 6

ameco6_vars <- c(
  "UVGD", "OVGD", "OVGDA", "OVGDQ", "UVGE", "OVGE"
)

ameco6_vars_trans <- c(
  B1GQ__CP_MNAC = "UVGD_0_0_0",
  B1GQ__CP_MEUR = "UVGD_99_0_0",
  B1GQ__CP_PPS = "UVGD_212_0_0",
  B1GQ__CLV15_MNAC = "OVGD_0_1_0",
  B1G__CP_MNAC = "UVGE_0_0_0",
  B1G__CP_MEUR = "UVGE_99_0_0",
  B1G__CP_PPS = "UVGE_212_0_0",
  B1G__CLV15_MNAC = "OVGE_0_1_0",
  gdp_ind_rel_ameco15 = "OVGDQ_0_0_415",
  gdp_ind_rel_ameco24 = "OVGDQ_0_0_424",
  gdp_ind_rel_ameco37 = "OVGDQ_0_0_437",
  B1GQA__CLV15_MNAC = "OVGDA_0_1_0")

ameco6 <- ameco6_0 %>%
  separate(code, into = c("geo", NA, "desc", "unit_code", "rel", "vars"), sep = "\\.", remove = FALSE) %>%
  filter(vars %in% ameco6_vars) %>%
  filter(desc %in% c(0,1)) %>%  # Only current values weights, 2 is PPS weights (it seems)
  unite(vars, vars, unit_code, desc, rel, sep = "_") %>%
  filter(vars != "UVGD_0_1_0")  %>%       # Remove country groups weighted mean (only standard aggregation)
  mutate_if(is.character, as_factor) %>%
  droplevels() %>%
  mutate(vars = fct_recode(vars, !!!ameco6_vars_trans))


# Table 7

ameco7_vars <- c(
  "UWCD", "PLCD", "PLCDQ", "QLCD", "QLCDQ"
)

ameco7_vars_trans <- c(
  D1__CP_MNAC = "UWCD_0_0_0",
  D1__CP_MEUR = "UWCD_99_0_0",
  D1__CP_PPS = "UWCD_212_0_0",
  nulc_aper = "PLCD_0_1_0",
  nulc_aper_eur = "PLCD_99_1_0",
  nulc_aper_rel_ameco15 = "PLCDQ_0_0_415",
  nulc_aper_rel_ameco24 = "PLCDQ_0_0_424",
  nulc_aper_rel_ameco37 = "PLCDQ_0_0_437",
  nulc_aper_usd_rel_ameco15 = "PLCDQ_30_0_415",
  nulc_aper_usd_rel_ameco24 = "PLCDQ_30_0_424",
  nulc_aper_usd_rel_ameco37 = "PLCDQ_30_0_437",
  rulc_aper = "QLCD_0_1_0",
  rulc_aper_rel_ameco15 = "QLCDQ_0_0_415",
  rulc_aper_rel_ameco24 = "QLCDQ_0_0_424",
  rulc_aper_rel_ameco37 = "QLCDQ_0_0_437"
  )



ameco7 <- ameco7_0 %>%
  separate(code, into = c("geo", NA, "desc", "unit_code", "rel", "vars"), sep = "\\.", remove = FALSE) %>%
  filter(vars %in% ameco7_vars) %>%
  filter(desc %in% c(0,1)) %>%  # Only current values weights, 2 is PPS weights (it seems)
  unite(vars, vars, unit_code, desc, rel, sep = "_") %>%
  filter(vars != "UWCD_0_1_0")  %>%       # Remove country groups weighted mean (only standard aggregation)
  mutate_if(is.character, as_factor) %>%
  droplevels() %>%
  mutate(vars = fct_recode(vars, !!!ameco7_vars_trans))

  # transmute(time = time,
  #           geo = countrycode::countrycode(geo, "iso3c", "eurostat", nomatch = NULL),
  #           # code = code,
  #           vars = fct_recode(title, !!!ameco7_vars),
  #           unit = fct_recode(unit, !!!ameco_units),
  #           values = values) %>%
  # unite(ind, vars, unit) %>%
  # mutate(ind = as_factor(ind),
  #        ind = gsub("_nac", "", ind)) %>%
  # group_by(geo, ind) %>%
  # mutate(values = rebase(values, time, baseyear = base_year)) %>%
  # ungroup() %>%
  # spread(ind, values)


# Table 9

ameco9_vars <-
  c("UXGS", "OXGS", "OXGSQ", # Export Goods and services
    "UXGN" , "OXGN",         # Goods
    "UXSN", "OXSN",         # Services
    "UMGS", "OMGS", "OMGSQ", # Import of goods and services
    "VXGSP")        # Market performance

ameco9_vars_trans <-
  c(P6__CP_MNAC = "UXGS_0_0_0",
    P6__CP_MEUR = "UXGS_99_0_0",
    P6__CLV15_MNAC = "OXGS_0_1_0",
    P7__CP_MNAC = "UMGS_0_0_0",
    P7__CP_MEUR = "UMGS_99_0_0",
    P7__CLV15_MNAC = "OMGS_0_1_0",
    exp_ind_rel_ameco15 = "OXGSQ_0_0_415",
    exp_ind_rel_ameco24 = "OXGSQ_0_0_424",
    exp_ind_rel_ameco37 = "OXGSQ_0_0_437",
    exp_perf_ameco= "VXGSP_0_0_0",
    P61__CP_MNAC = "UXGN_0_0_0",
    P61__CP_MEUR = "UXGN_99_0_0",
    P61__CLV15_MNAC = "OXGN_0_1_0",
    P62__CP_MNAC = "UXSN_0_0_0",
    P62__CP_MEUR = "UXSN_99_0_0",
    P62__CLV15_MNAC = "OXSN_0_1_0")


ameco9 <- ameco9_0 %>%
  separate(code, into = c("geo", NA, "desc", "unit_code", "rel", "vars"), sep = "\\.", remove = FALSE) %>%
  filter(vars %in% ameco9_vars) %>%
  filter(desc %in% c(0,1)) %>%  # Only current values weights, 2 is PPS weights (it seems)
  mutate_if(is.character, as_factor) %>%
  droplevels() %>%
  unite(vars, vars, unit_code, desc, rel, sep = "_") %>%
  filter(!vars %in% c("UXGS_0_1_0", "UMGS_0_1_0", "UXGN_0_1_0", "UXSN_0_1_0")) %>% # Remove other than standard aggregation for country groups
  mutate(vars = fct_recode(vars, !!!ameco9_vars_trans))

# unique(ameco9$vars)

# Table 10

ameco10_vars <-
  c("UBGS") # Net exports G & S

ameco10_vars_trans <-
  c(B11__CP_MNAC = "UBGS_0_0_0")

ameco10 <- ameco10_0 %>%
  separate(code, into = c("geo", NA, "desc", "unit_code", "rel", "vars"), sep = "\\.", remove = FALSE) %>%
  filter(vars %in% ameco10_vars) %>%
  filter(desc %in% c(0,1)) %>%  # Only current values weights, 2 is PPS weights (it seems)
  mutate_if(is.character, as_factor) %>%
  droplevels() %>%
  unite(vars, vars, unit_code, desc, rel, sep = "_") %>%
  filter(vars %in% ameco10_vars_trans) %>%
  mutate(vars = fct_recode(vars, !!!ameco10_vars_trans))

# All together

data_ameco_raw <-
  bind_rows(ameco1, ameco6, ameco7, ameco9, ameco10) %>%
  mutate(geo = as_factor(countrycode::countrycode(geo, "iso3c", "eurostat",
                                                  nomatch = NULL,
                                                  custom_match = c(ROM = "RO"))))

data_ameco <-
  data_ameco_raw %>%
  select(geo, time, vars, values) %>%
  spread(vars, values) %>%
  mutate(across(contains(c("MNAC", "MEUR", "PPS")),  ~1000 * .x))

ameco_long_geos <- data_ameco %>%
  group_by(geo) %>%
  summarise(is_60 = !is.na(nulc_aper[time == 1960])) %>%
  ungroup() %>%
  filter(is_60) %>%
  pull(geo)

data_ameco_long <-
  data_ameco %>%
  filter(geo %in% ameco_long_geos)


usethis::use_data(data_ameco, data_ameco_long, ameco_long_geos, overwrite = TRUE)

write.csv2(data_ameco_long, file = "data-out/data_ameco_long.csv")
saveRDS(data_ameco_long, file = "data-out/data_ameco_long.rds")


