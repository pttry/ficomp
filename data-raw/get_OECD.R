# Get OECD data

library(OECD)
library(dplyr)
library(forcats)
library(lubridate)

# dataset_list <- get_datasets()
# search_dataset("Quarterly National Accounts", data = dataset_list)
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
      "National currency, chained volume estimates, national reference year, quarterly levels, seasonally adjusted",
      "Hours worked, seasonally adjusted",
      "Jobs, seasonally adjusted"
    )
  ) %>%
  pull(id)

geo <- c("FIN", "USA")

oecd_dat_Q_0 <- get_dataset(dataset = dataset_id,
                          filter = list(geo, subject_list, measure_list, "Q"))

oecd_dat_Q <- oecd_dat_Q_0 %>%
  mutate_if(is.character, as_fa)

