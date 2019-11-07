# Get Conference Board International Comparisons of Manufacturing Productivity and Unit Labor Costs - Data
# https://www.conference-board.org/ilcprogram/index.cfm?id=30139
# Adjusted flat file downloaded

library(dplyr)
library(forcats)
library(readr)
library(here)

ilc_flat0 <-
  readr::read_tsv(
    here("data-raw/ILCProductivityULCAllDataJul2018.txt"),
    col_types = cols(
      COUNTRY = col_factor(),
      INDUSTRY = col_factor(),
      YEAR = col_integer(),
      `Nominal Value Added` = col_number(),
      `Real Value Added` = col_number(),
      Employment = col_number(),
      `Total Hours` = col_number(),
      `Total Labor Cost` = col_number(),
      `National Currency per US dollar` = col_double(),
      `Consumer Price Index (2010=100)` = col_double(),
      X11 = col_skip()
    )
  )

col_names0 = c(
  COUNTRY = "country",
  INDUSTRY = "industry",
  YEAR = "year",
  `Nominal Value Added` = "va_cp",
  `Real Value Added` = "va_fp",
  Employment = "emp",
  `Total Hours` = "hws",
  `Total Labor Cost` = "lc",
  `National Currency per US dollar` = "nac_usd",
  `Consumer Price Index (2010=100)` = "cpi10"
)

col_names <- setNames(names(col_names0), col_names0)


# There are errors in data deliminators, they are removed
ilc_dat <- ilc_flat0[-problems(ilc_flat0)$row,] %>%
  rename(!!!col_names)

usethis::use_data(ilc_dat, overwrite = TRUE)






