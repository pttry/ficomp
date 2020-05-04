# Get AMECO data
# https://ec.europa.eu/info/business-economy-euro/indicators-statistics/economic-databases/macro-economic-database-ameco/download-annual-data-set-macro-economic-database-ameco_en

# AMECO - Gross Domestic Product (Income Approach), Labour Costs

library(readr)
library(dplyr)
library(tidyr)

ameco_lc_link <- "http://ec.europa.eu/economy_finance/db_indicators/ameco/documents/ameco7.zip"

tempf <- tempfile(fileext = "zip")
download.file(ameco_lc_link, tempf)

ameco_file <- unzip(tempf, "AMECO7.TXT")


ameco_lc <- readr::read_delim(
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

usethis::use_data(ameco_lc, overwrite = TRUE)

library(ggplot2)
library(ggptt)

ameco_lc %>%
  filter(sub_chapter == "07 Nominal unit labour costs, total economy",
         country == "Finland",
         time >= 1995) %>%
  ggplot(aes(time, values, colour = unit)) +
  geom_line() +
  facet_wrap(~ title)
  the_legend_bot()



  # distinct(title, unit)
# %>% pull(title)
