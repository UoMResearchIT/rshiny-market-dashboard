# R Shiny Market and Economic Dashboard

This repository contains an R Shiny dashboard for displaying market and economic data.

## Features

- Stock price lookup using Yahoo Finance
- Inflation, unemployment and national debt data from the World Bank API
- Brent crude oil data from FRED
- Bitcoin, gold, silver and VIX data from Yahoo Finance
- UK claimant count data from the Office for National Statistics
- Yahoo Finance RSS news feed

## Required R packages

```r
install.packages(c("shiny", "quantmod", "DT", "xml2", "jsonlite"))
