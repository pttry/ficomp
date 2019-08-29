# ficomp

<!-- badges: start -->
<!-- badges: end -->

The goal of ficomp is to ...

## Data collection


### National account data 

- Eurostat (data-raw/get_eurostat.R)
  + Eu data
  + Selected international quarterly/annual data. Whole economy.
  + namq_10_gdp, namq_10_a10
    - unit
      + Current prices, million units of national currency
      + Chain linked volumes (2010), million units of national currency
    -  Seasonally and calendar adjusted
    - na_item
      + Gross domestic product at market prices, 
      + "Value added, gross", 
      + "Compensation of employees"
      + "Wages and salaries" 
      + "Employers' social contributions" 
  
- OECD
  + OECD countries + some other


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

 

## Report

### Terms of trade adjusted GDP

see vignettes/ULC_tot.Rmd

## Shiny app


