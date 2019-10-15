# ficomp

<!-- badges: start -->
<!-- badges: end -->

The goal of ficomp is to ...

## Data collection

- Country codes: eurostat, euroarea EA

### National account data 

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
  
### ULC data  
  
- OECD (data-raw/get_OECD.R)
  + Quartely data only per persons
  + OECD countries + some other
  
- ECB (data-raw/get_ECB.R)
  + based on eurostat data


### Weights

#### BIS double trade weights

Source: [BIS Effective exchange rate indices](https://www.bis.org/statistics/eer.htm)

Data: 
* Narrow: weights_bis_narrow
* Broad: weights_bis_broad

Update: data-raw(get_BIS.R)

Vignette: Weights.Rmd

TODO: document, test

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

## Report

### Terms of trade adjusted GDP

see vignettes/ULC_tot.Rmd

## Shiny app


