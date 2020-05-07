# Get AMECO data
# https://ec.europa.eu/info/business-economy-euro/indicators-statistics/economic-databases/macro-economic-database-ameco/download-annual-data-set-macro-economic-database-ameco_en

# AMECO - Gross Domestic Product (Income Approach), Labour Costs

library(readr)
library(dplyr)
library(tidyr)
library(forcats)

ameco_lc_link <- "http://ec.europa.eu/economy_finance/db_indicators/ameco/documents/ameco7.zip"

tempf <- tempfile(fileext = "zip")
download.file(ameco_lc_link, tempf)

ameco_file <- unzip(tempf, "AMECO7.TXT")


ameco_lc_0 <- readr::read_delim(
  ameco_file, delim = ";",
  col_types = cols(
    CODE = col_character(),
    COUNTRY = col_factor(),
    "SUB-CHAPTER" = col_factor(),
    TITLE = col_factor(),
    UNIT = col_factor(),
    .default = col_number())) %>%
  select(-starts_with("X")) %>%
  statfitools::clean_names(to_lower = TRUE) %>%
  gather(time, values, starts_with("x")) %>%
  mutate(time = as.numeric(gsub("x", " ", time)))

ameco_vars <- c(
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

ameco_lc <- ameco_lc_0 %>%
  filter(title %in% ameco_vars,
         unit %in% ameco_units) %>%
  separate(code, into = c("geo", NA, "desc", NA, NA, "vars"), sep = "\\.", remove = FALSE) %>%
  filter(desc %in% c(0,1)) %>%  # Only current values weights, 2 is PPS weights (it seems)
  droplevels() %>%
  transmute(time = time,
            geo = countrycode::countrycode(geo, "iso3c", "eurostat", nomatch = NULL),
            # code = code,
            vars = fct_recode(title, !!!ameco_vars),
            unit = fct_recode(unit, !!!ameco_units),
            values = values) %>%
  unite(ind, vars, unit) %>%
  mutate(ind = as_factor(ind),
         ind = gsub("_nac", "", ind)) %>%
  group_by(geo, ind) %>%
  mutate(values = rebase(values, time, baseyear = base_year)) %>%
  ungroup() %>%
  spread(ind, values)

usethis::use_data(ameco_lc, overwrite = TRUE)

