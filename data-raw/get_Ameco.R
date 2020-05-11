# Get AMECO data
# https://ec.europa.eu/info/business-economy-euro/indicators-statistics/economic-databases/macro-economic-database-ameco/download-annual-data-set-macro-economic-database-ameco_en

# AMECO - Gross Domestic Product (Income Approach), Labour Costs

library(readr)
library(dplyr)
library(tidyr)
library(forcats)

## Read Ameco tables from https://ec.europa.eu/info/business-economy-euro/indicators-statistics/economic-databases/macro-economic-database-ameco/download-annual-data-set-macro-economic-database-ameco_en

# warnings because ameco files are mal formated.

# Domestic Product
ameco6_0 <- read_ameco(table_num = 6)
# Gross Domestic Product (Income Approach), Labour Costs
ameco7_0 <- read_ameco(table_num = 7)
# Exports and Imports of Goods And Services, National Accounts
ameco9_0 <- read_ameco(table_num = 9)





ameco7_vars <- c(
  nulc_aper = "Nominal unit labour costs: total economy (Ratio of compensation per employee to real GDP per person employed.)",
  nulc_aper_rel_ameco  = "Nominal unit labour costs: total economy :- Performance relative to the rest of 24 industrial countries: double export weights (Ratio of compensation per employee to real GDP per person employed.)",
  nulc_aper_relEU_ameco = "Nominal unit labour costs: total economy :- Performance relative to the rest of the former EU-15: double export weights (Ratio of compensation per employee to real GDP per person employed.)",
  nulc_aper_rel37_ameco = "Nominal unit labour costs: total economy :- Performance relative to the rest of 37 industrial countries: double export weights (Ratio of compensation per employee to real GDP per person employed.)"
)

ameco_units <- c(
  eur = "(EUR: 2015 = 100) ",
  usd = "(USD: 2015 = 100) ",
  nac = "(National currency: 2015 = 100) "
)

ameco_lc <- ameco7_0 %>%
  filter(title %in% ameco7_vars,
         unit %in% ameco_units) %>%
  separate(code, into = c("geo", NA, "desc", NA, NA, "vars"), sep = "\\.", remove = FALSE) %>%
  filter(desc %in% c(0,1)) %>%  # Only current values weights, 2 is PPS weights (it seems)
  droplevels() %>%
  transmute(time = time,
            geo = countrycode::countrycode(geo, "iso3c", "eurostat", nomatch = NULL),
            # code = code,
            vars = fct_recode(title, !!!ameco7_vars),
            unit = fct_recode(unit, !!!ameco_units),
            values = values) %>%
  unite(ind, vars, unit) %>%
  mutate(ind = as_factor(ind),
         ind = gsub("_nac", "", ind)) %>%
  group_by(geo, ind) %>%
  mutate(values = rebase(values, time, baseyear = base_year)) %>%
  ungroup() %>%
  spread(ind, values)

ameco9_vars <-
  c("UXGS", "OXGS", "OXGSQ", # Goods and services
    "VXGSP", "VMGSW",        # Market performance
    "UXGN" , "OXGN",         # Goods
    "UXSN", "OXSN")

ameco_unit_codes <-
  c(MNAC = )

ameco9 <- ameco9_0 %>%
  separate(code, into = c("geo", NA, "desc", "unit_code", NA, "vars"), sep = "\\.", remove = FALSE) %>%
  filter(vars %in% ameco9_vars) %>%
  filter(desc %in% c(0)) %>%  # Only current values weights
  mutate_if(is.character, as_factor) %>%
  droplevels()



unique(ameco9$vars)



usethis::use_data(ameco_lc, overwrite = TRUE)

