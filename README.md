
# Rnssp

<!-- badges: start -->
[![Rnssp Pipeline](https://github.com/cdcent/Rnssp/actions/workflows/check-standard.yaml/badge.svg?branch=master)](https://github.com/cdcent/Rnssp/actions/workflows/check-standard.yaml)
<!-- badges: end -->

The **Rnssp** R package is a catalog of data processing and analytics tools, templates, and functions commonly used across the National Syndromic and Surveillance Program at the Centers for Disease Control and Prevention (CDC). Its goal is to improve code reproducibility, standardize and document reusable functions, facilitate the sharing of routine reports across the NSSP Community of Practice (CoP), and better engage the NSSP CoP.

## Installation

You can install the development version of `Rnssp` from GitHub:

```r
# install the Rnssp package
devtools::install_github("cdcent/Rnssp")
```


## Usage

This is a basic example which shows you how to:

- use the `Rnssp` package to create an NSSP user profile
- and use the NSSP user profile to pull data from ESSENCE via the ESSENCE API


``` r
library(Rnssp)

## Creating an NSSP user profile
myProfile <- Credentials$new(
  username = askme("Enter your username: "), 
  password = askme()
)

## Inspect your `myProfile` confirming that username and password are completely hidden
myProfile

## JSON URL from ESSENCE API
url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/alerts/regionSyndromeAlerts?end_date=31Jan2021&start_date=29Jan2021"

## Pull Time Series Data from ESSENCE
api_data <- myProfile$get_api_data(url)

## Inspect data object structure
names(api_data)

## Get a glimpse of the pulled dataset
glimpse(api_data$regionSyndromeAlerts) 
```

## Contributing to this project
Should you want to contribute to this project, submit a push request to this Github repository and consider submitting a request to be added as a developer to gazondekon@cdc.gov.

## Getting Help
If you encounter a clear bug, please consider emailing the author at gazondekon@cdc.gov and/or file an issue with a minimal reproducible example.
