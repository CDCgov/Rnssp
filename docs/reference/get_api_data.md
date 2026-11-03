# Get API data

A wrapper to the `get_api_data` method defined in the
[`Credentials`](https://cdcgov.github.io/Rnssp/reference/Credentials.md),
[`Token`](https://cdcgov.github.io/Rnssp/reference/Token.md), and
[`Apikey`](https://cdcgov.github.io/Rnssp/reference/Apikey.md) classes.

## Usage

``` r
get_api_data(url, fromCSV = FALSE, profile = myProfile, ...)
```

## Arguments

- url:

  a character of API URL.

- fromCSV:

  a logical, defines whether data are returned in .csv format or .json
  format.

- profile:

  an object of class
  [`Auth`](https://cdcgov.github.io/Rnssp/reference/Auth.md). Default is
  `myProfile`.

- ...:

  further arguments and CSV parsing parameters to be passed to
  [`read_csv`](https://readr.tidyverse.org/reference/read_delim.html)
  when `fromCSV = TRUE`.

## Value

a dataframe (`fromCSV = TRUE`) or a list containing a dataframe and its
metadata (`fromCSV = TRUE`).

## See also

[`get_api_response`](https://cdcgov.github.io/Rnssp/reference/get_api_response.md)
and
[`get_api_graph`](https://cdcgov.github.io/Rnssp/reference/get_api_graph.md)

## Examples

``` r
if (FALSE) { # \dontrun{
myProfile <- create_profile("", "")

json_url <- "http://httpbin.org/json"
api_data_json <- get_api_data(json_url)
head(api_data_json$slideshow$slides)

csv_url <- "http://httpbin.org/robots.txt"
api_data_csv <- get_api_data(csv_url, fromCSV = TRUE)
head(api_data_csv)
} # }
```
