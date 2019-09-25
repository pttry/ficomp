# Get foreign trade from Uljas and statfin databases

library(dplyr)
library(forcats)
library(pxweb)
library(uljas)
library(statfitools)
library(eurostat)
library(countrycode)

################################
## Trade on goods from Uljas

# cn_dims <- uljas_dims(ifile = "/DATABASE/01 ULKOMAANKAUPPATILASTOT/01 CN/ULJAS_CN")
# cn_dims[[1]]$label
# names(cn_dims)
# cn_class <- uljas_class(ifile = "/DATABASE/01 ULKOMAANKAUPPATILASTOT/01 CN/ULJAS_CN", class = "Classification of Products CN2")
# ind_class <- uljas_class(ifile = "/DATABASE/01 ULKOMAANKAUPPATILASTOT/01 CN/ULJAS_CN", class = "Indicators")
# flow_class <- uljas_class(ifile = "/DATABASE/01 ULKOMAANKAUPPATILASTOT/01 CN/ULJAS_CN", class = "Flow")
# country_class <- uljas_class(ifile = "/DATABASE/01 ULKOMAANKAUPPATILASTOT/01 CN/ULJAS_CN", class = "Country")

# All groups, all quarters, imports by origin and export by destination, all countries, Value (euro)
cn_query <- list(`Classification of Products CN2` = c("00 - 99"), Quarter = "=ALL", Flow = c("1", "2"), Country = "=ALL", Indicators = "V1")
cn_data <- uljas_data(ifile = "/DATABASE/01 ULKOMAANKAUPPATILASTOT/01 CN/ULJAS_CN", classifiers = cn_query)

fi_goods_trade_dat <- cn_data %>%
  transmute(geo = statfitools::extract_code(Country),
            geo = countrycode(geo, "iso2c", "eurostat", nomatch = NULL, custom_match = c(AA = "TOTAL")),
            geo = as_factor(geo),
            flow = fct_recode(Flow,
                              imp_g ="Imports by countries of origin",
                              exp_g = "Exports by countries of destination"),
            time = eurostat::eurotime2date(Quarter),
            values = values)

#################################
## Trade on services from statfi

# d <- pxweb_interactive("statfi")


px_service_trade_dat <-
  pxweb_get(url = "http://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/kan/tpulk/statfin_tpulk_pxt_004.px",
            query = list("Alue"=c("*"),
                         "Tiedot"=c("Tuonti","Vienti"),
                         "Vuosi"=c("*"))) %>%
  as.data.frame(column.name.type = "code", variable.value.type = "code") %>%
  statfitools::clean_times(time_format = "date") %>%
  statfitools::clean_names(to_lower = TRUE, rename_values = TRUE)



fi_service_trade_dat <- px_service_trade_dat %>%
  transmute(geo = countrycode(alue, "iso2c", "eurostat", nomatch = NULL, custom_match = c(TOT = "TOTAL")),
            flow = fct_recode(tiedot,
                              imp_s ="Tuonti",
                              exp_s = "Vienti"),
            time = time,
            values = values)




#################################
# save
usethis::use_data(fi_goods_trade_dat, fi_service_trade_dat, overwrite = TRUE)

