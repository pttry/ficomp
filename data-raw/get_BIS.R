## Weights data from BIS statistics

library(dplyr)
library(tidyr)
library(readxl)
library(purrr)

wbis_narrow_path <- "https://www.bis.org/statistics/eer/weightsn.xlsx"
wbis_broad_path <- "https://www.bis.org/statistics/eer/weightsb.xlsx"

wbis_narrow_temp <- tempfile(fileext = ".xlsx")
download.file(wbis_narrow_path, wbis_narrow_temp, mode = "wb")
wbis_broad_temp <- tempfile(fileext = ".xlsx")
download.file(wbis_broad_path, wbis_broad_temp, mode = "wb")

wbis_narrow_sheets <- readxl::excel_sheets(wbis_narrow_temp)
names(wbis_narrow_sheets) <- wbis_narrow_sheets
wbis_broad_sheets <- readxl::excel_sheets(wbis_broad_temp)
names(wbis_broad_sheets) <- wbis_broad_sheets

weights_bis_narrow <-
  map_dfr(wbis_narrow_sheets, ~readxl::read_xlsx(wbis_narrow_temp, sheet = ., range = "B6:AC33"), .id = "time") %>%
  rename(geo_base = ...1) %>%
  gather(geo, weight, -geo_base, - time) %>%
  mutate(geo_base = countrycode::countrycode(geo_base, "iso2c", "eurostat", custom_match = c(XM = "EA")),
         geo = countrycode::countrycode(geo, "iso2c", "eurostat", custom_match = c(XM = "EA")),
         time_range = time) %>%
  tidyr::separate(time, into = c("time1", "time2"), sep = "_", convert = TRUE) %>%
  mutate(time = (time1 + time2) / 2) %>%
  complete(geo_base, geo, time) %>%
  group_by(geo_base, geo) %>%
  tidyr::fill(weight, weight, .direction = "updown") %>%
  ungroup()

weights_bis_broad <-
  map_dfr(wbis_broad_sheets, ~readxl::read_xlsx(wbis_broad_temp, sheet = ., range = "B6:BN70"), .id = "time") %>%
  rename(geo_base = ...1) %>%
  gather(geo, weight, -geo_base, - time) %>%
  mutate(geo_base = countrycode::countrycode(geo_base, "iso2c", "eurostat", custom_match = c(XM = "EA")),
         geo = countrycode::countrycode(geo, "iso2c", "eurostat", custom_match = c(XM = "EA")),
         time_range = time) %>%
  tidyr::separate(time, into = c("time1", "time2"), sep = "_", convert = TRUE) %>%
  mutate(time = (time1 + time2) / 2) %>%
  complete(geo_base, geo, time) %>%
  group_by(geo_base, geo) %>%
  tidyr::fill(weight, weight, .direction = "updown") %>%
  ungroup()


if (!("US"%in% weights_bis_broad$geo)) message("Check xlsx range in broad, US is misssing")
if (!("US"%in% weights_bis_narrow$geo)) message("Check xlsx range in narrow, US is misssing")

usethis::use_data(weights_bis_narrow, overwrite = TRUE)
usethis::use_data(weights_bis_broad, overwrite = TRUE)


