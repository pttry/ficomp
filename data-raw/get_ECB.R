# Get data from ECB

library(dplyr)
library(forcats)
library(ecb)
library(httr)
library(lubridate)
library(countrycode)

# ecb_stats <- get_dataflows()
# ecb_stats %>%
#   filter(flow_ref == "MNA")

# Unit labour cost series are in the dataset: MNA National accounts, Main aggregates (Eurostat ESA2010 TP, table 1)
# to get a whole content:
# mna0 <- httr::GET("https://sdw-wsrest.ecb.europa.eu/service/data/MNA", accept("text/csv"))
# mna_res <- httr::content(mna0, "parsed") %>%
#   mutate_if(is.character, as_factor)


# Unit labour cost per hours and person
# Q, SA, all countries,
ecb_ulc0 <- get_data("MNA.Q.Y..W2.S1.S1._Z.ULC_HW+ULC_PS._Z._T._Z.IX.D.N")

ulc_ecb_dat <- ecb_ulc0 %>%
  transmute(time = yq(obstime),
            geo = as_factor(countrycode(ref_area,
                                        origin = "ecb",
                                        destination = "eurostat",
                                        custom_match = c(B4 = "EU27", B5 = "EU28", B6 = "EU27b",
                                                         I6 = "EA17", I7 = "EA18", I8 = "EA19"))),
            na_item = factor(sto,
                             levels = c("ULC_HW", "ULC_PS"),
                             labels = c("NULC_HW", "NULC_PER")),
            values = obsvalue)


usethis::use_data(ulc_ecb_dat, overwrite = TRUE)


# Trade weights

ecb_stats %>%
  filter(grepl("weight", title, ignore.case = "TRUE"))

wts0 <- httr::GET("https://sdw-wsrest.ecb.europa.eu/service/data/WTS", accept("text/csv"))
wts_res <- httr::content(wts0, "parsed") %>%
  mutate_if(is.character, as_factor)

kk <- wts_res %>%
  filter(REF_AREA == "FI",
         COUNT_AREA == "SE",
         TRADE_WEIGHT == "O") %>%
  droplevels()

# # Check also unit labour cost based competiveness indicators (also inflation and gdp deflators)
#
# ecb_stats %>%
#   filter(grepl("unit", title, ignore.case = "TRUE"))
#
# kk <- httr::GET("https://sdw-wsrest.ecb.europa.eu/service/data/JDF_EXR_HCI_ULCT", accept("text/csv"))
# res <- httr::content(kk, "parsed")
#
# res2 <- res %>%
#   mutate_if(is.character, as_factor)
#
# levels(res2$TITLE)
#
# res2 %>%
#   filter(TITLE == "ULCT deflated HCI-19/Finnish markka",
#          EXR_SUFFIX == "A") %>%
#   select(KEY, TIME_PERIOD, OBS_VALUE) %>%
#   droplevels() %>%
#   mutate(time = lubridate::yq(TIME_PERIOD)) %>%
#   ggplot(aes(time, OBS_VALUE, colour = KEY)) +
#   geom_line()







