# Update data used in package

devtools::load_all()

# Get NA and ULC data from sources:

#  Eurostat
#  - Q NA
#  - A NA
#  - Exchange rates, nominal and effective
source("data-raw/get_eurostat.R")


#  OECD
#  - ULC
#  - Q NA
#  - A NA
source("data-raw/get_OECD.R")

#  AMECO
#  - Used for extending data
#  - long A NA
source("data-raw/get_Ameco.R")

#  ILC
#  - Additional data to extend industry data
#  - Source data file have to be downloaded manually
# source("data-raw/get_ILC.R")


# Get weights:
source("data-raw/get_ECFIN_weights.R")
source("data-raw/get_BIS.R")
# IMF weight should be asked by email
# source("data-raw/get_IMF_weights.R")

# Get extra data

#  OECD EO
#  - Export  market indicators
#  - Additional long ULC series
source("data-raw/get_OECD_EO.R")

#  ECB
#  - Additional ULC and weights data from ECB
#  - not needed for updates
source("data-raw/get_ECB.R")

#  ECI
#  - addtional data for complexity of economy


# Update metadata if changed
# source("data-raw/data_meta.R")


# Update data computations
source("data-raw/data_quaterly.R")
source("data-raw/data_annual.R")
