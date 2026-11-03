# Get API response

A wrapper to the `get_api_response` method defined in the
[`Auth`](https://cdcgov.github.io/Rnssp/reference/Auth.md) class.

## Usage

``` r
get_api_response(url, profile = myProfile)
```

## Arguments

- url:

  a character of API URL

- profile:

  an object of class
  [`Auth`](https://cdcgov.github.io/Rnssp/reference/Auth.md). Default is
  `myProfile`.

## Value

An object of class `response`.

## See also

`get_api_response` and
[`get_api_graph`](https://cdcgov.github.io/Rnssp/reference/get_api_graph.md)

## Examples

``` r
if (FALSE) { # \dontrun{
myProfile <- create_profile("", "")
url <- "http://httpbin.org/json"

api_response <- get_api_response(url)
names(api_response)
} # }
```
