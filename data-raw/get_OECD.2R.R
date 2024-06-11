# Get OECD data

library(OECD)
library(dplyr)
library(tidyr)
library(forcats)
library(lubridate)
library(countrycode)
library(purrr)

devtools::load_all()

start_year <- 1995

# dataset_list <- get_datasets()





## Unit labour cost
# ULC_QUA is discontinued

# search_dataset("Unit", data = dataset_list)

# HUOM esim Japanille Jobs pidempi sarja

# Unit labour costs and labour productivity (employment based), Total economy
# https://stats.oecd.org/Index.aspx?DataSetCode=ULC_EEQ

# ulc_id <- "ULC_EEQ"

# ulc_str <- get_data_structure(ulc_id)

ulc_subjects <- c(
  NULC_APER = "ULCE",
  D1_EMP_APER = "LCEMP",
  GDP_EMP_PER = "GDPEMP"
)

ulc_filter <- pRoductivity::make_oecd_filter(
  list("",              # geo
       "Q",             #
       ulc_subjects,    # Meusure
       "",
       "IX",            # index
       "",
       "",
       "S",              # Seasonal adjust
       ""
       )
)

ulc_oecd_dat0 <-
  get_dataset("OECD.SDD.TPS,DSD_PDB@DF_PDB_ULC_Q,1.0",
              filter = ulc_filter)

ulc_oecd_dat <- ulc_oecd_dat0 %>%
  transmute(
    time = yq(TIME_PERIOD),
    geo = as_factor(countrycode(REF_AREA, "iso3c", "eurostat", nomatch = NULL)),
    na_item = fct_recode(MEASURE, !!!ulc_subjects),
    unit = as_factor("I15"),
    values = as.numeric(ObsValue))


usethis::use_data(ulc_oecd_dat, overwrite = TRUE)

# ulc_oecd_dat_old <- ulc_oecd_dat
# usethis::use_data(ulc_oecd_dat_old, overwrite = TRUE)

## National account

## Quarterly National Accounts

# search_dataset("Quarterly National Accounts", data = dataset_list)

# qna_id <- "QNA"
#
# qna_str <- get_data_structure(qna_id)

# qna_str$SUBJECT %>% View()

qna_subjects <- c(
  B1GQ = "B1GQ", #"Gross domestic product",
  D1 = "D1", #"Compensation of employees",
  P6 = "P6", #"Exports of goods and services",
  P61 = "P61", #"Exports of goods",
  P62 = "P62", #"Export of services"
  B11 = "B11") #"External balance of goods and services"

# industry level data, manufacturing. Not use because not avaible for US and Japan
# gna_subjects_ind <- c(
#   # B1GxTOTAL = "B1G",  # Gross value added at basic prices, total activity
#   # B1GxC = "B1GVC",    # Gross value added at basic prices, manufacturing
#   # D1xTOTAL = "D1",    #"Compensation of employees, total",
#   # D1xC = "D1VC",      #"Compensation of employees, manufacturing",
#   EMP_DC__TH_PERxTOTAL = "ETO",  # Employment, total
#   EMP_DC__TH_PERxC = "ETOVC"  # Employment, manufacturing
# )

qna_measures <-
  c(
    CP_MNAC = "V",   # National currency, current prices, quarterly levels, seasonally adjusted
    CLV_MNAC = "LR"  # National currency, chained volume estimates, national reference year, quarterly levels, seasonally adjusted
  )


qna_geo <- countrycode(oecd_geos, "eurostat", "iso3c", nomatch = NULL)
gna_cur <- countrycode(oecd_geos, "eurostat", "iso4217c", nomatch = NULL)


# oecd_dat_Q_0 <- get_dataset(dataset = qna_id,
#                           filter = list(qna_geo, qna_subjects, qna_measures, "Q"))
qna_filter <- pRoductivity::make_oecd_filter(
  list(
    "Q",             # Quaterly
    "Y",             # SA
    qna_geo,
    "",
    "",
    qna_subjects,
    "_Z",
    c("_Z", "_T"),
    "",
    "",
    qna_measures,
    "",
    c("T0103", "T0102")# Table

  )
)
oecd_dat_Q_0 <- get_dataset("OECD.SDD.NAD,DSD_NAMAIN1@DF_QNA,1.0",
                            filter = qna_filter)

oecd_dat_Q <- oecd_dat_Q_0 %>%
  transmute(
    time = yq(TIME_PERIOD),
    geo = as_factor(countrycode(REF_AREA, "iso3c", "eurostat", nomatch = NULL)),
    na_item = fct_recode(TRANSACTION, !!!qna_subjects),
    unit = fct_recode(PRICE_BASE, !!!qna_measures),
    currency = as_factor(CURRENCY),
    values = as.numeric(ObsValue)) %>%
  distinct() |>
  filter(!(na_item == "B11" & unit == "CLV_MNAC")) |>  # constant p balance not needed
  filter(currency != "_Z") |>     # drop index version
  droplevels()


usethis::use_data(oecd_dat_Q, overwrite = TRUE)
# usethis::use_data(oecd_dat_Q_old, overwrite = TRUE)




## Annual national accounts

# search_dataset("SNA", data = dataset_list) %>% View()

# dataset_list %>% filter(grepl("SNA", id)) %>% View()

# sna1_str <- get_data_structure("SNA_TABLE1")
# # sna6_str <- get_data_structure("SNA_TABLE6")
# sna6a_str <- get_data_structure("SNA_TABLE6A")
# sna7a_str <- get_data_structure("SNA_TABLE7A")

# sna1_str$TRANSACT %>% knitr::kable()
# sna6a_str$TRANSACT %>% knitr::kable()
# sna7a_str$MEASURE %>% knitr::kable()

# Countries
sna_geo <- countrycode(c("US", "JP"), "eurostat", "iso3c")

# Industries
sna_activity <- setNames(names(main_nace_sna_new), main_nace_sna_new)

# Transactions

sna1_transact <- c(
  B1GQ = "B1GQ", #"Gross domestic product",
  # D1 = "D1", #"Compensation of employees, total",
  P6 = "P6", #"Exports of goods and services",
  P61 = "P61", #"Exports of goods",
  P62 = "P62", #"Exports of services",
  B11 = "B11") #"External balance of goods and services"

sna1_2_transact <- c(
  D1 = "D1") #"Compensation of employees, total",


sna6a_transact <- c(
  B1G = "B1G", #"Gross Value added",
  D1 = "D1" #"Compensation of employees, total",
)

sna7a_transact <- c(
  EMP_DC = "EMP", # "Total empoyment",
  SAL_DC = "SAL" #"Employees",
)


# Measeures
sna_measures <-   c(
  CP_MNAC = "V",   # current prices
  CLV15_MNAC = "LR",  # Constant prices
  PYP_MNAC = "Y" # previous year prices
)

sna7a_measures <-   c(
  THS_PER = "PS",   # Persons
  THS_HW = "H",   # Hours
  THS_JOBS = "JB"  # JObs
)


filter_sna1 <- list("A", sna_geo, "", "", sna1_transact, "", "", "", "XDC", sna_measures, "", "")
dat_oecd_sna1_0 <- get_dataset(dataset = "OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE1_EXPENDITURE,1.0",
                               filter = pRoductivity::make_oecd_filter(filter_sna1))

filter_sna1_2 <- list("A", sna_geo, "", "", sna1_2_transact, "", "_T", "", "XDC", sna_measures["CP_MNAC"], "", "")
dat_oecd_sna1_2_0 <- get_dataset(dataset = "OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE1_INCOME,1.0",
                                 filter = pRoductivity::make_oecd_filter(filter_sna1_2))

filter_sna6 <- list("A", sna_geo, "", "", sna6a_transact, "", sna_activity, "", "", sna_measures, "", "")
dat_oecd_sna6a_0 <- get_dataset(dataset = "OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE6,1.0",
                                filter = pRoductivity::make_oecd_filter(filter_sna6))

# dat_oecd_sna7a_0 <- get_dataset(dataset = "SNA_TABLE7A",
#                                 filter = list(sna_geo, sna7a_transact, sna_activity, sna7a_measures))

filter_sna7a <- list("A", sna_geo, "", "", sna7a_transact, "", sna_activity, "", sna7a_measures, "", "", "")
dat_oecd_sna7a_0 <- get_dataset(dataset = "OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE7,1.0",
                                filter = pRoductivity::make_oecd_filter(filter_sna7a))


dat_oecd_sna1_1 <- dat_oecd_sna1_0 %>%
  transmute(
    time = as.numeric(TIME_PERIOD),
    geo = as_factor(countrycode(REF_AREA, "iso3c", "eurostat", nomatch = NULL)),
    na_item = fct_recode(TRANSACTION, !!!sna1_transact),
    unit = fct_recode(PRICE_BASE, !!!sna_measures),
    currency = as_factor(CURRENCY),
    values = as.numeric(ObsValue))  %>%
  mutate(nace_r2 = "TOTAL")  %>%
  unite(vars, na_item, unit, sep = "__") %>%
  mutate(vars = as_factor(vars)) %>%
  spread(vars, values) %>%
  filter(time >= start_year) %>%
  droplevels() %>%
  complete(time, geo)

dat_oecd_sna1_2 <- dat_oecd_sna1_2_0 %>%
  transmute(
    time = as.numeric(TIME_PERIOD),
    geo = as_factor(countrycode(REF_AREA, "iso3c", "eurostat", nomatch = NULL)),
    na_item = fct_recode(TRANSACTION, !!!sna1_2_transact),
    unit = fct_recode(PRICE_BASE, !!!sna_measures["CP_MNAC"]),
    currency = as_factor(CURRENCY),
    values = as.numeric(ObsValue))  %>%
  mutate(nace_r2 = "TOTAL")  %>%
  unite(vars, na_item, unit, sep = "__") %>%
  mutate(vars = as_factor(vars)) %>%
  spread(vars, values) %>%
  filter(time >= start_year) %>%
  droplevels() %>%
  complete(time, geo)

dat_oecd_sna6a <- dat_oecd_sna6a_0 %>%
  transmute(
    time = as.numeric(TIME_PERIOD),
    geo = as_factor(countrycode(REF_AREA, "iso3c", "eurostat", nomatch = NULL)),
    nace_r2 = fct_recode(ACTIVITY, !!!sna_activity),
    na_item = fct_recode(TRANSACTION, !!!sna6a_transact),
    unit = fct_recode(PRICE_BASE, !!!sna_measures),
    currency = as_factor(CURRENCY),
    values = as.numeric(ObsValue))  %>%
  unite(vars, na_item, unit, sep = "__") %>%
  mutate(vars = as_factor(vars)) %>%
  spread(vars, values)

dat_oecd_sna7a <- dat_oecd_sna7a_0 %>%
  transmute(
    time = as.numeric(TIME_PERIOD),
    geo = as_factor(countrycode(REF_AREA, "iso3c", "eurostat", nomatch = NULL)),
    nace_r2 = fct_recode(ACTIVITY, !!!sna_activity),
    na_item = fct_recode(TRANSACTION, !!!sna7a_transact),
    unit = fct_recode(UNIT_MEASURE, !!!sna7a_measures),
    values = as.numeric(ObsValue))  %>%
  unite(vars, na_item, unit, sep = "__") %>%
  mutate(vars = as_factor(vars)) %>%
  spread(vars, values) |>
  mutate(SAL_DC__THS_HW = 1000 * SAL_DC__THS_HW,
         EMP_DC__THS_HW = 1000 * EMP_DC__THS_HW)


dat_oecd_sna_nace <-
  dat_oecd_sna6a %>%
  left_join(dat_oecd_sna7a, by = c("time", "geo", "nace_r2")) %>%
  filter(time >= start_year) %>%
  complete(time, geo, nace_r2)


dat_oecd_sna_nace_imput <-
  dat_oecd_sna_nace %>%
  # Extrapolate emp based on jobs
  nest(data = c(-geo, -nace_r2)) %>%
  mutate(emp_mod = map(data, possibly(~lm(EMP_DC__THS_PER ~ EMP_DC__THS_JOBS, data = .x), otherwise = NA_real_)),
         emp_approx = map2(emp_mod, data, possibly(~predict(.x, .y), otherwise = NA_real_)),
         sal_mod = map(data, possibly(~lm(SAL_DC__THS_PER ~ SAL_DC__THS_JOBS, data = .x), otherwise = NA_real_)),
         sal_approx = map2(sal_mod, data, possibly(~predict(.x, .y), otherwise = NA_real_))) %>%
  select(-emp_mod, -sal_mod) %>%
  unnest(cols = c(data, emp_approx, sal_approx)) %>%
  mutate(EMP_DC__THS_PER = coalesce(EMP_DC__THS_PER, emp_approx),
         SAL_DC__THS_PER = coalesce(SAL_DC__THS_PER, sal_approx)) %>%
  select(-emp_approx, -sal_approx) %>%
  # Approx EMP_DC__THS_HW base on EMP_DC__THS_PER and SAL_DC__THS_HW and SAL_DC__THS_PER
  mutate(EMP_DC__THS_HW = if_else(is.na(EMP_DC__THS_HW), EMP_DC__THS_PER * SAL_DC__THS_HW / SAL_DC__THS_PER, EMP_DC__THS_HW)) %>%
  # drop M and N for Japan for missing
  filter(!(geo == "JP" & nace_r2 %in% c("M", "N")))

# visdat::vis_dat(dat_oecd_sna_nace)

data_oecd_sna_nace_a <-
  dat_oecd_sna_nace_imput %>%
  mutate_at(vars(matches("CP_MNAC")),
            .funs = list(EUR = ~EUR(., time, currency, exh_eur_a))) %>%
  rename_at(vars(contains("MNAC_EUR")), list(~gsub("NAC_", "", .))) %>%
  filter(time >= a_start_time)

data_oecd_sna_a <-
  dat_oecd_sna1_1 %>%
  left_join(dat_oecd_sna1_2, by = c("time", "geo", "currency", "nace_r2")) %>%
  mutate_at(vars(matches("CP_MNAC")),
            .funs = list(EUR = ~EUR(., time, currency, exh_eur_a))) %>%
  rename_at(vars(contains("MNAC_EUR")), list(~gsub("NAC_", "", .))) %>%
  filter(time >= a_start_time)



# data_oecd_sna_a %>%
#   filter(nace_r2 == "TOTAL") %>%
#   select(geo, time, starts_with("D1")) %>%
#   gather(vars, values, -geo, -time) %>%
#   ggplot(aes(time, values, colour = vars)) +
#   facet_wrap(~geo, scales = "free") +
#   geom_line()


usethis::use_data(data_oecd_sna_nace_a, data_oecd_sna_a, overwrite = TRUE)

