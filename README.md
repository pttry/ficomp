# ficomp

<!-- badges: start -->
<!-- badges: end -->

The goal of ficomp is to ...

## Data collection

- Country codes: eurostat, euroarea EA

### National account data 

- GDP
- Import and export
- Trade and service balance


- Eurostat (data-raw/get_eurostat.R)
  + Data: naq_eurostat_dat
  + Eu data
  + Selected international quarterly/annual data. Whole economy.
  + namq_10_gdp, namq_10_a10
    - unit
      + Current prices, million units of national currency
      + Chain linked volumes (2010), million units of national currency
    -  Seasonally and calendar adjusted, except some data seasonally adjusted (DE, FR compensation data)
    - na_item
      + Gross domestic product at market prices, 
      + "Value added, gross", 
      + "Compensation of employees"
      + "Wages and salaries" 
      + "Employers' social contributions" 
    - nace_r2
      + Total
      + Manufacturing
      + TODO: Open / private sector
      
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
