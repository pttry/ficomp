# ficomp

<!-- badges: start -->
<!-- badges: end -->

The goal of ficomp is to ...

## Data collection

Data files are in data-raw -folder:

Final data is parsed togerther in files:

### data_meta.R  - Country groups, dates, labels and other metadata

### data-raw/data_annual.R  - Final annual data 

data.frames:

data_main_groups_a
- Value added based data with industry groups

data_main_total_a

- Total economy indicators like gdp, export, import and EO indicators of export markets

data_main_annual

- Main annual data for total economy, based on eurostat and ameco data (non-eurostat countries).

data_long_annual

- Longer annual data for total economy, based on eurostat and ameco data (non-eurostat countries and for missing eurostat data).

 
### data-raw/data_quartely.R  - Final quartely data 
  
data.frames:

q_dat 
- Eurostat and OECD QNA data added with OECD ULC and EO indicators from eo_q_dat.
- Countries: eurostat_geo and oecd_geo

q_dat_oecd_ulc  
- OECD ULC data
- eurostat_geos, oecd_geos_ulcq

data_quartely_est
- As q_dat but added countries / year from estimated quaterly data based on AMECO annual data (data_ameco).

data_main_groups_q
- Industries: TOTAL and C
- Only Eurostat data

data_quartely_manu_est



### Data source files

- Eurostat (data-raw/get_eurostat.R)
  + Quaterly national accounts: 
    + data.frames:
      + naq_eurostat_dat Total and C
      + naq_eurostat_nace_dat 6 industries
      + naq_eruostat_dat_raw (internal use)
      + ulc_eurostat_dat unit labour cost data
    + Seasonal adjusted (SCA or SA)
    + Source tables: namq_10_gdp, namq_10_a10, namq_10_a10_e, namq_10_lp_ulc
      - unit
        + Current prices, million units of national currency
        + Chain linked volumes (2010), million units of national currency
      - na_item
        + Gross domestic product at market prices, 
        + "Value added, gross", 
        + "Compensation of employees"
        + "Wages and salaries" 
        + "Employers' social contributions" 
  + Annual national accounts
    + data.frames:
      + data_eurostat_nama_nace_a,
      + data_eurostat_nama_nace10_a,
      + data_eurostat_nama_a       
    + Source tables: nama_10_gdp, nama_10_a10, nama_10_a10_e, nama_10_a64, nama_10_a64_e


- OECD (data-raw/get_OECD.R)
  + [Unit labour costs and labour productivity database](https://stats.oecd.org/Index.aspx?DataSetCode=ULC_EEQ)
  + Quartely data only per persons
  + OECD countries + some other
  
- OECD Economic Outlook (data-raw/get_OECD_EO.R)
  + IS NOT UPDATED as latest does not include all variables (due to COVID ?)

- OECD STAN (data-raw/get_OECD_STAN.R)
  + not used

- ECB (data-raw/get_ECB.R)
  + based on eurostat data
  
- AMECO (data-raw/get_Ameco)
  + yearly
  + data_ameco, data_ameco_long

- The Conference Board International Labor Comparisons Program (ILC) (data-raw/get_ILC.R).
  + (https://www.conference-board.org/ilcprogram/index.cfm?id=30139)
  + Yearly
  + industry 




### National account data 

- GDP
- Import and export
- Trade and service balance



      
- OECD 
  + Quaterly nationanl accounts (QNA)
    - Only total economy. Industry data available only mainly EU countries.
    - Transformed to same base year as Eurostat 2015
    - Calculated euros for current price series
    - Employment series are missing for USA and Japan
  + Annual national accounts
  + Economic outlook data
  
  
  
### ULC data  
  
- OECD (data-raw/get_OECD.R)
  + [Unit labour costs and labour productivity database](https://stats.oecd.org/Index.aspx?DataSetCode=ULC_EEQ)
  + Quartely data only per persons
  + OECD countries + some other
  
- ECB (data-raw/get_ECB.R)
  + based on eurostat data
  
- AMECO (data-raw/get_Ameco)
  + yearly
  + data_ameco, data_ameco_long

- The Conference Board International Labor Comparisons Program (ILC) (data-raw/get_ILC.R).
  + (https://www.conference-board.org/ilcprogram/index.cfm?id=30139)
  + Yearly
  + industry 

### EER data 

- Nominal and real effective exchange rates from Eurostat - DG ECFIN
  + data-raw/get_eurostat.r
  + vignettes/EER.Rmd

### Other data

- [The Economic Complexity Index](https://oec.world/en/rankings/country/eci/) 


### Weights

In weightning missing weights are extrapolated or if not possible treated as zeros.

#### BIS double trade weights

Source: [BIS Effective exchange rate indices](https://www.bis.org/statistics/eer.htm)

Data: 
* Narrow: weights_bis_narrow
* Broad: weights_bis_broad

Update: data-raw/get_BIS.R

Vignette: Weights.Rmd

TODO: document, test

#### IMF EER weights

Source: email STA-EERWeights@imf.org

Data: weights_imf 

Update: data-raw/get_IMF_weights.R

Note:
* Missing yearly weights are extraploted from closest data. In earlier weights data treshold for inclusion was higher (1%) than in more resent (0.45%)

#### Export

* Good and services
  + [Statistics Finland](http://tilastokeskus.fi/til/tpulk/). 
    + 7 countries 2015-2019, Q
    + Services: 260 countries 2015-2017, Y

* Goods export
  + [UN Comtrade Database](https://comtrade.un.org/) - [comtradr](https://cran.r-project.org/web/packages/comtradr/)
  + Custums Finland
  
#### Country groups
  
  * geo21
  * geo20 for ilc as nz missing.

## Indicator functions

 * ind_ulc() Nominal unit labour costs

## Indicators
 * lc_dat
   + ulc nominal unit labour cost
   + ulc_adj nominal unit labour cost, adjusted to take account emploees/emplyed share

## Model data

* Q and A
* also components

## Report

### Terms of trade adjusted GDP

see vignettes/ULC_tot.Rmd

## Shiny app



## TODO

Why Greece missing
