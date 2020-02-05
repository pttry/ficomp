# Get OECD data

library(OECD)
library(dplyr)
library(tidyr)
library(forcats)
library(lubridate)
library(countrycode)

# dataset_list <- get_datasets()

## Unit labour cost
# ULC_QUA is discontinued

# search_dataset("Unit", data = dataset_list)



# Unit labour costs and labour productivity (employment based), Total economy
# https://stats.oecd.org/Index.aspx?DataSetCode=ULC_EEQ
ulc_str <- get_data_structure("ULC_EEQ")


ulc_oecd_dat0 <-
  get_dataset("ULC_EEQ",
              filter = list(
                ulc_str$LOCATION$id,
                ulc_str$SUBJECT$id,
                "IXOBSA",             # Index, seasonally adjusted
                "Q"))

ulc_oecd_dat <- ulc_oecd_dat0 %>%
  transmute(
    time = yq(obsTime),
    geo = as_factor(countrycode(LOCATION, "iso3c", "eurostat", nomatch = NULL)),
    na_item = factor(SUBJECT,
                     levels = c("ULQEUL01", "ULQECU01", "ULQELP01"),
                     labels = c("NULC_APER", "D1_EMP_APER", "GDP_EMP_PER")),
    unit = as_factor("I15"),
    values = obsValue)


usethis::use_data(ulc_oecd_dat, overwrite = TRUE)


## National account

## Quarterly National Accounts

# search_dataset("Quarterly National Accounts", data = dataset_list)

qna_id <- "QNA"

qna_str <- get_data_structure(qna_id)

qna_subjects <- c(
  B1GQ = "B1_GS1", #"Gross domestic product",
  D1 = "D1S1", #"Compensation of employees, total",
  P6 = "P6", #"Exports of goods and services",
  P61 = "P61", #"Exports of goods",
  B11 = "B11") #"External balance of goods and services"

qna_measures <-
  c(
    CP_NAC = "CQRSA",   # National currency, current prices, quarterly levels, seasonally adjusted
    CLV_NAC = "LNBQRSA"  # National currency, chained volume estimates, national reference year, quarterly levels, seasonally adjusted
  )


qna_geo <- countrycode(oecd_geos, "eurostat", "iso3c", nomatch = NULL)

oecd_dat_Q_0 <- get_dataset(dataset = qna_id,
                          filter = list(qna_geo, qna_subjects, qna_measures, "Q"))

# kk <- get_dataset(dataset = qna_id,
#                   filter = list("USA", "B1_GS1"), start_time = 2017) %>%
#   mutate_if(is.character, as_factor)
# #
# # qna_str$SUBJECT %>%
# #   filter(id %in% unique(kk$SUBJECT)) %>% # View()
# #   filter(grepl("alue" , label))
# #
# mm <- kk %>%
#   filter(FREQUENCY == "Q") %>% distinct(MEASURE) %>% pull()
#
# qna_str$MEASURE %>%
#   filter(id %in% mm) %>% View()
                #
# k <- get_dataset(dataset = dataset_id,
#                  filter = list("USA", "GDP", "CQRSA", "Q"))

oecd_dat_Q <- oecd_dat_Q_0 %>%
  transmute(
    time = yq(obsTime),
    geo = as_factor(countrycode(LOCATION, "iso3c", "eurostat", nomatch = NULL)),
    na_item = fct_recode(SUBJECT, !!!qna_subjects),
    unit = fct_recode(MEASURE, !!!qna_measures),
    currency = as_factor(UNIT),
    values = obsValue)


usethis::use_data(oecd_dat_Q, overwrite = TRUE)


## National accounts

# search_dataset("SNA", data = dataset_list) %>% View()

dataset_list %>% filter(grepl("SNA", id)) %>% View()

sna1_str <- get_data_structure("SNA_TABLE1")

sna1_str$TRANSACT %>% View()

sna_transact <- c(
  B1GQ = "B1_GA", #"Gross domestic product",
  D1 = "D11", #"Compensation of employees, total",
  P6 = "P6", #"Exports of goods and services",
  P61 = "P61", #"Exports of goods",
  B11 = "B11") #"External balance of goods and services"


sna_measures <-   c(
  CP_NAC = "C",   # current prices
  CLV_NAC = "V"  # Constant prices
)

# Needed countries that are in SNA
sna_geo <- intersect(
    countrycode(c(eurostat_geos, oecd_geos), "eurostat", "iso3c"),
    sna1_str$LOCATION$id)


oecd_dat_sna_0 <- get_dataset(dataset = "SNA_TABLE1",
                            filter = list(sna_geo, sna_transact, sna_measures))

oecd_dat_sna <- oecd_dat_sna_0 %>%
  transmute(
    time = as.numeric(obsTime),
    geo = as_factor(countrycode(LOCATION, "iso3c", "eurostat", nomatch = NULL)),
    na_item = fct_recode(TRANSACT, !!!sna_transact),
    unit = fct_recode(MEASURE, !!!sna_measures),
    currency = as_factor(UNIT),
    values = obsValue)



usethis::use_data(oecd_dat_sna, overwrite = TRUE)
