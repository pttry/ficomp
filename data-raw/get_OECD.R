# Get OECD data

library(OECD)
library(dplyr)
library(forcats)
library(lubridate)

dataset_list <- get_datasets()
# search_dataset("Quarterly National Accounts", data = dataset_list)
search_dataset("weights", data = dataset_list)
dataset_id <- "QNA"


stan_str <- get_data_structure(dataset_id)

subject_list <- stan_str$SUBJECT %>%
  filter(
    label %in% c(
      "Gross domestic product",
      "Gross value added at basic prices, total activity",
      "Compensation of employees, total",
      "Total employment",
      "Employees, total"
    )
  ) %>%
  pull(id)


measure_list <- stan_str$MEASURE %>%
  filter(
    label %in% c(
      "National currency, current prices, quarterly levels, seasonally adjusted",
      "Volume index, OECD reference year, seasonally adjusted",
      "Hours worked, seasonally adjusted",
      "Jobs, seasonally adjusted"
    )
  ) %>%
  pull(id)

geo <- c("FIN", "USA")

oecd_dat_Q_0 <- get_dataset(dataset = dataset_id,
                          filter = list(geo, subject_list, measure_list, "Q"))

oecd_dat_Q_test <- get_dataset(dataset = dataset_id,
                            filter = list("USA"), start_time = "2015", end_time = "2015")

test <- oecd_dat_Q_test %>%
  filter(obsTime == "2015-Q1")

stan_str$SUBJECT %>%
  left_join(select(test, id = SUBJECT, obsValue)) %>% View()

oecd_dat_Q <- oecd_dat_Q_0 %>%
  mutate_if(is.character, as_factor) %>%
  filter(obsTime == "2015-Q1")

table(is.na(oecd_dat_Q$obsValue))

# Main economic indicators
mei_str <- get_data_structure("MEI")
# View(mei_str$SUBJECT)

# ULC

ulc_str <- get_data_structure("ULC_EEQ")

ulc_oecd_dat0 <-
  get_dataset("ULC_EEQ",
               filter = list(
                 ulc_str$LOCATION$id,
                 ulc_str$SUBJECT$id,
                 "IXOBSA",             # Index, seasonally adjusted
                 "Q"))

ulc_oecd_dat <- ulc_oecd_dat0 %>%
  mutate_if(is.character, as_factor) %>%
  mutate(time = yq(obsTime))


ulc_oecd_dat %>%
  filter(LOCATION %in% c("FIN", "USA", "SWE"),
         SUBJECT == "ULQEUL01") %>%
  ggplot(aes(time, obsValue, colour = LOCATION)) +
  geom_line()
