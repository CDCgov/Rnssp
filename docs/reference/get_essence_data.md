# Get ESSENCE data

Get ESSENCE API data.

## Usage

``` r
get_essence_data(
  url,
  start_date = NULL,
  end_date = NULL,
  profile = myProfile,
  ...
)
```

## Arguments

- url:

  a character of ESSENCE API URL.

- start_date:

  a date object or a character string in date format (e.g. "2019-08-01")

- end_date:

  a date object or a character string in date format (e.g. "2020-08-01")

- profile:

  an object of class
  [`Auth`](https://cdcgov.github.io/Rnssp/reference/Auth.md). Default is
  `myProfile`.

- ...:

  further arguments and CSV parsing parameters to be passed to
  [`read_csv`](https://readr.tidyverse.org/reference/read_delim.html)
  when `fromCSV = TRUE`.

## Value

a dataframe or a character string.

## See also

[`get_api_response`](https://cdcgov.github.io/Rnssp/reference/get_api_response.md),
[`get_api_data`](https://cdcgov.github.io/Rnssp/reference/get_api_data.md)
and
[`get_api_graph`](https://cdcgov.github.io/Rnssp/reference/get_api_graph.md)

## Examples

``` r
if (FALSE) { # \dontrun{
myProfile <- create_profile(askme("Enter your username:"), askme())

# Example 1
url <- "https://essence2.syndromicsurveillance.org/nssp_essence/api/timeSeries?
endDate=20Sep22&ccddCategory=cli%20cc%20with%20cli%20dd%20and%20coronavirus%20dd%20v2
&percentParam=ccddCategory&geographySystem=hospitaldhhsregion&datasource=va_hospdreg
&detector=probrepswitch&startDate=22Jun22&timeResolution=daily&hasBeenE=1
&medicalGroupingSystem=essencesyndromes"

url <- url %>% gsub("\n", "", .)

## Pull ESSENCE data
api_data <- get_essence_data(url)

## Inspect data
glimpse(api_data)


# Example 2
url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/alerts/regionSyndromeAlerts?
end_date=31Jan2021&start_date=29Jan2021"

url <- url %>% gsub("\n", "", .)

## Pull last 30 days of ESSENCE data
api_data <- get_essence_data(url, start_date = Sys.Date() - 30, end_date = Sys.Date())

## Inspect data
glimpse(api_data)


# Example 3
url <- "https://essence2.syndromicsurveillance.org/nssp_essence/api/timeSeries/graph?
endDate=25Jun2022&geography=&percentParam=noPercent&datasource=va_hosp&startDate=25Jun2021
&medicalGroupingSystem=essencesyndromes&userId=3751&aqtTarget=TimeSeries&ccddCategory=
&geographySystem=hospitalregion&detector=probrepswitch&timeResolution=daily"

url <- url %>% gsub("\n", "", .)

## Pull Time series graph
api_tsgraph <- get_essence_data(url)

## Preview time series graph
graph <- imager::load.image(api_tsgraph)
plot(graph)
} # }
```
