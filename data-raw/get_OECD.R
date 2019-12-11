# Get OECD data

library(OECD)
library(dplyr)
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

# search_dataset("Quarterly National Accounts", data = dataset_list)

qna_id <- "QNA"

qna_str <- get_data_structure(qna_id)

qna_subjects <- c(
  "B1_GS1", #"Gross domestic product",
  "D1S1", #"Compensation of employees, total",
  "P6", #"Exports of goods and services",
  "P61", #"Exports of goods",
  "B11") #"External balance of goods and services"

qna_measures <- qna_str$MEASURE %>%
  filter(
    label %in% c(
      "National currency, current prices, quarterly levels, seasonally adjusted",
      "Volume index, OECD reference year, seasonally adjusted"
    )
  ) %>%
  pull(id)

qna_geo <- countrycode(oecd_geos, "eurostat", "iso3c", nomatch = NULL)

oecd_dat_Q_0 <- get_dataset(dataset = qna_id,
                          filter = list(qna_geo, qna_subjects, qna_measures, "Q"))

# kk <- get_dataset(dataset = dataset_id,
#                   filter = list("USA"), start_time = 2017) %>%
#   mutate_if(is.character, as_factor)
#
# qna_str$SUBJECT %>%
#   filter(id %in% unique(kk$SUBJECT)) %>% # View()
#   filter(grepl("alue" , label))
#
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
    na_item = as_factor(SUBJECT),
    unit = as_factor(MEASURE),
    currency = as_factor(UNIT),
    values = obsValue)



#
# oecd_dat_Q_test <- get_dataset(dataset = dataset_id,
#                             filter = list("USA"), start_time = "2015", end_time = "2015")
#
# test <- oecd_dat_Q_test %>%
#   filter(obsTime == "2015-Q1")
#
# stan_str$SUBJECT %>%
#   left_join(select(test, id = SUBJECT, obsValue)) %>% View()
#
# oecd_dat_Q <- oecd_dat_Q_0 %>%
#   mutate_if(is.character, as_factor) %>%
#   filter(obsTime == "2015-Q1")
#
# table(is.na(oecd_dat_Q$obsValue))
#
# # Main economic indicators
# mei_str <- get_data_structure("MEI")
# # View(mei_str$SUBJECT)

