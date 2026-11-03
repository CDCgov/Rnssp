# Utility to Change the Start and/Or End Dates in an ESSENCE API URL

Change the start and/or end dates in an ESSENCE API URL

## Usage

``` r
change_dates(url, start_date = NULL, end_date = NULL)
```

## Arguments

- url:

  a character string of ESSENCE API URL

- start_date:

  a date object or a character string in date format (e.g. "2019-08-01")

- end_date:

  a date object or a character string in date format (e.g. "2020-08-01")

## Value

a character string

## Examples

``` r
library(Rnssp)

url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/alerts/hospitalSyndromeAlerts?
end_date=31Jan2021&start_date=29Jan2021"

url <- url %>% gsub("\n", "", .)

# Change start date to January 15, 2021
url %>% change_dates(start_date = "2021-01-15")

# Change end date to February 15, 2021
url %>% change_dates(end_date = "2021-02-15")

# Change start date to January 15, 2021 and end date to February 15, 2021
url %>% change_dates(start_date = "2021-01-15", end_date = "2021-02-15")

# Change end and start dates to respectively "current" and "current - 7 days"
url %>% change_dates(start_date = Sys.Date() - 7, end_date = Sys.Date())
```
