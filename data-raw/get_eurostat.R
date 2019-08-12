## Data from Eurostat database

library(dplyr)
library(eurostat)

naq_eurostat <- get_eurostat("namq_10_gdp", cache = FALSE)

naq_eurostat_dat <- naq_eurostat %>%
  # 	Current prices, million units of national currency ,  Chain linked volumes (2010), million units of national currency
  #  	Seasonally and calendar adjusted data
  # "Gross domestic product at market prices", "Value added, gross", "Compensation of employees", "Wages and salaries", "Employers' social contributions"
  filter(unit %in% c("CP_MNAC", "CLV10_MNAC"),
         s_adj == "SCA",
         na_item %in% c("B1GQ", "B1G", "D1", "D11", "D12")
  )


usethis::use_data(naq_eurostat_dat, overwrite = TRUE)
