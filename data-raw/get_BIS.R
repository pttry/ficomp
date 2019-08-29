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
  rename(geo = ...1) %>%
  gather(geo2, weight, -geo, - time)

weights_bis_broad <-
  map_dfr(wbis_broad_sheets, ~readxl::read_xlsx(wbis_broad_temp, sheet = ., range = "B6:BJ66"), .id = "time") %>%
  rename(geo = ...1) %>%
  gather(geo2, weight, -geo, - time)


usethis::use_data(weights_bis_narrow, overwrite = TRUE)
usethis::use_data(weights_bis_broad, overwrite = TRUE)

