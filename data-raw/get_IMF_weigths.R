## Weights data from IMF

# xlsx-file from email

library(dplyr)
library(tidyr)
library(readxl)
library(purrr)
library(stringr)
library(forcats)

imf_file <- here::here("data-raw/IMF_Weights_Complete_Set.xlsx")

sheets <- readxl::excel_sheets(imf_file)
sheets <- set_names(sheets, make.names(sheets))

weights_imf <- map(sheets, ~read_xlsx(imf_file, .x)) %>%
  bind_rows() %>%
  transmute(geo_base = countrycode::countrycode(str_sub(Series_code, 1, 3), "imf", "eurostat",
                                                custom_match = c("163" = "EA"), nomatch = NULL),
         geo = countrycode::countrycode(str_sub(Series_code, -3, -1), "imf", "eurostat",
                                        custom_match = c("163" = "EA"), nomatch = NULL),
         time1 = as.numeric(str_sub(Series_code, 11, 14)),
         time2 = as.numeric(str_sub(Series_code, 16, 19)),
         time = (time1 + if_else(is.na(time2), time1 + 2, time2)) / 2,
         time_range = paste0(time1, "_", time2),
         weight = Weights) %>%
  filter(geo_base %in% all_extra_geos, geo %in% all_extra_geos) %>%
  complete(geo_base, geo, time) %>%
  group_by(geo_base, geo) %>%
  tidyr::fill(weight, weight, .direction = "updown") %>%
  ungroup()


usethis::use_data(weights_imf, overwrite = TRUE)
