

load_all()

q_start_time <- "1995-01-01"

## Countries

ea_countries <- eurostat::ea_countries$code
eu_countries<- eurostat::eu_countries$code
other_eurostat_countries <- c("NO", "CH", "IS")
agg_eurostat <- c("EA19", "EU28")

eurostat_geos <- c(eu_countries, other_eurostat, agg_eurostat)

#9 other industrial countries (Australia, Canada, United States, Japan, Norway, New Zealand, Mexico, Switzerland and Turkey)
IC37_other <- c("AU", "CA", "US", "JP", "NO", "NZ", "MX", "CH", "TR")
exec_countries <- c("CL", "CR", "IL", "IS", "KR")
other_oecd <- c("AU", "CA", "US", "JP", "NO", "NZ", "CH")


oecd_geos <- c("AU", "CA", "US", "JP", "NZ", "CH")

# setdiff(unique(ulc_eurostat_dat$geo), eurostat::eu_countries$code)
#
# setdiff(eurostat::eu_countries$code, unique(stan_dat$geo))
#
# eurostat::label_eurostat(kk, dic = "geo")
# eurostat::label_eurostat(IC37_other, dic = "geo")


usethis::use_data(eurostat_geos, oecd_geos)

## Quarterly data for ULC
# Only for person based

# Data from eurostat
#  source("data-raw/eurostat.R")
#  in addition to EU countries also "NO", "CH", "IS"
data("naq_eurostat_dat")

# and from OECD
#  source("data-raw/get_OECD.R")
data("ulc_oecd_dat")

q_dat <-


## Annual data

data("stan_dat")
