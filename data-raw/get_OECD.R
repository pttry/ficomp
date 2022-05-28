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

ulc_id <- "ULC_EEQ"

ulc_str <- get_data_structure(ulc_id)

ulc_subjects <- c(
  NULC_APER = "ULQEUL01",
  D1_EMP_APER = "ULQECU01",
  GDP_EMP_PER = "ULQELP01"
)

ulc_oecd_dat0 <-
  get_dataset(ulc_id,
              filter = list(
                ulc_str$LOCATION$id,
                ulc_subjects,
                "IXOBSA",             # Index, seasonally adjusted
                "Q"))

ulc_oecd_dat <- ulc_oecd_dat0 %>%
  transmute(
    time = yq(Time),
    geo = as_factor(countrycode(LOCATION, "iso3c", "eurostat", nomatch = NULL)),
    na_item = fct_recode(SUBJECT, !!!ulc_subjects),
    unit = as_factor("I15"),
    values = as.numeric(ObsValue))


usethis::use_data(ulc_oecd_dat, overwrite = TRUE)


## National account

## Quarterly National Accounts

# search_dataset("Quarterly National Accounts", data = dataset_list)

qna_id <- "QNA"

qna_str <- get_data_structure(qna_id)

# qna_str$SUBJECT %>% View()

qna_subjects <- c(
  B1GQ = "B1_GS1", #"Gross domestic product",
  D1 = "D1S1", #"Compensation of employees",
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
    CP_MNAC = "CQRSA",   # National currency, current prices, quarterly levels, seasonally adjusted
    CLV_MNAC = "LNBQRSA"  # National currency, chained volume estimates, national reference year, quarterly levels, seasonally adjusted
  )


qna_geo <- countrycode(oecd_geos, "eurostat", "iso3c", nomatch = NULL)

oecd_dat_Q_0 <- get_dataset(dataset = qna_id,
                          filter = list(qna_geo, qna_subjects, qna_measures, "Q"))



oecd_dat_Q <- oecd_dat_Q_0 %>%
  transmute(
    time = yq(Time),
    geo = as_factor(countrycode(LOCATION, "iso3c", "eurostat", nomatch = NULL)),
    na_item = fct_recode(SUBJECT, !!!qna_subjects),
    unit = fct_recode(MEASURE, !!!qna_measures),
    currency = as_factor(UNIT),
    values = as.numeric(ObsValue)) %>%
  filter(!(na_item == "B11" & unit == "CLV_MNAC")) # constant p balance not needed


usethis::use_data(oecd_dat_Q, overwrite = TRUE)




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
sna_geo <- countrycode(c(oecd_geos), "eurostat", "iso3c")

# Industries
sna_activity <- setNames(names(main_nace_sna), main_nace_sna)

sna1_transact <- c(
  B1GQ = "B1_GA", #"Gross domestic product",
  D1 = "D11", #"Compensation of employees, total",
  P6 = "P6", #"Exports of goods and services",
  P61 = "P61", #"Exports of goods",
  P62 = "P62", #"Export of services"
  B11 = "B11") #"External balance of goods and services"

sna6a_transact <- c(
  B1G = "B1GA", #"Gross Value added",
  D1 = "D1A" #"Compensation of employees, total",
  )

sna7a_transact <- c(
  EMP_DC = "ETOA", # "Total empoyment",
  SAL_DC = "EEMA" #"Employees",
)



sna_measures <-   c(
  CP_MNAC = "C",   # current prices
  CLV15_MNAC = "V",  # Constant prices
  PYP_MNAC = "VP" # previous year prices
)

sna7a_measures <-   c(
  THS_PER = "PER",   # current prices
  THS_HW = "HRS",  # Constant prices
  THS_JOBS = "JOB"  # JObs
)




dat_oecd_sna1_0 <- get_dataset(dataset = "SNA_TABLE1",
                            filter = list(sna_geo, sna1_transact, sna_measures))

# oecd_dat_sna6_0 <- get_dataset(dataset = "SNA_TABLE6",
#                               filter = list(sna_geo, sna6_transact, sna6_activity, sna_measures))

dat_oecd_sna6a_0 <- get_dataset(dataset = "SNA_TABLE6A",
                               filter = list(sna_geo, sna6a_transact, sna_activity, sna_measures))

dat_oecd_sna7a_0 <- get_dataset(dataset = "SNA_TABLE7A",
                                filter = list(sna_geo, sna7a_transact, sna_activity, sna7a_measures))




dat_oecd_sna <- dat_oecd_sna1_0 %>%
  transmute(
    time = as.numeric(Time),
    geo = as_factor(countrycode(LOCATION, "iso3c", "eurostat", nomatch = NULL)),
    na_item = fct_recode(TRANSACT, !!!sna1_transact),
    unit = fct_recode(MEASURE, !!!sna_measures),
    currency = as_factor(UNIT),
    values = as.numeric(ObsValue)) %>%
  mutate(nace_r2 = "TOTAL")  %>%
  unite(vars, na_item, unit, sep = "__") %>%
  mutate(vars = as_factor(vars)) %>%
  spread(vars, values)

dat_oecd_sna6a <- dat_oecd_sna6a_0 %>%
  transmute(
    time = as.numeric(Time),
    geo = as_factor(countrycode(LOCATION, "iso3c", "eurostat", nomatch = NULL)),
    nace_r2 = fct_recode(ACTIVITY, !!!sna_activity),
    na_item = fct_recode(TRANSACT, !!!sna6a_transact),
    unit = fct_recode(MEASURE, !!!sna_measures),
    currency = as_factor(UNIT),
    values = as.numeric(ObsValue))  %>%
  # Korjaa Japanin puuttuva currency osassa tiedoista
  mutate(currency = if_else(is.na(currency) & geo == "JP", factor("JPY", levels = c("JPY", "USD")), currency)) |>
  #
  unite(vars, na_item, unit, sep = "__") %>%
  mutate(vars = as_factor(vars)) %>%
  spread(vars, values)

dat_oecd_sna7a <- dat_oecd_sna7a_0 %>%
  transmute(
    time = as.numeric(Time),
    geo = as_factor(countrycode(LOCATION, "iso3c", "eurostat", nomatch = NULL)),
    nace_r2 = fct_recode(ACTIVITY, !!!sna_activity),
    na_item = fct_recode(TRANSACT, !!!sna7a_transact),
    unit = fct_recode(MEASURE, !!!sna7a_measures),
    values = as.numeric(ObsValue)) %>%
  unite(vars, na_item, unit, sep = "__") %>%
  mutate(vars = as_factor(vars)) %>%
  spread(vars, values)

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
  dat_oecd_sna %>%
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

