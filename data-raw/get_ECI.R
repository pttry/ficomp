# Economic complexity index
# https://oec.world/en/rankings/country/eci/

# see also: https://pacha.hk/economiccomplexity/index.html

library(tidyverse)
library(countrycode)

eci_dat0 <- readr::read_csv("https://oec.world/en/rankings/country/eci/?download=true&download_all=true")

eci_dat0 %>%
  distinct(Country, `Country ID`) %>% View()

countrycode::codelist %>% View()

eci_dat <- eci_dat0 %>%
  transmute(time = Year,
            country = Country,
            geo = countrycode(substr(`Country ID`, 3, 5), "iso3c", "eurostat"),
            eci = ECI,
            eci_plus = `ECI+`)

usethis::use_data(eci_dat, overwrite = TRUE)

# eci_dat %>%
#   ggplot(aes(time, eci, group = geo)) +
#   geom_line(colour = "grey50") +
#   geom_line(data = filter(eci_dat, geo == "FI"), colour = "blue")
