# Get an API graph

A wrapper to the `get_api_graph` method defined in the
[`Credentials`](https://cdcgov.github.io/Rnssp/reference/Credentials.md),
[`Token`](https://cdcgov.github.io/Rnssp/reference/Token.md), and
[`Apikey`](https://cdcgov.github.io/Rnssp/reference/Apikey.md) classes.

## Usage

``` r
get_api_graph(url, file_ext = ".png", profile = myProfile)
```

## Arguments

- url:

  a character of API URL.

- file_ext:

  a non-empty character vector giving the file extension. Default is
  `.png`.

- profile:

  an object of class
  [`Auth`](https://cdcgov.github.io/Rnssp/reference/Auth.md). Default is
  `myProfile`.

## Value

An object of class `response`.

## See also

[`get_api_response`](https://cdcgov.github.io/Rnssp/reference/get_api_response.md)
and
[`get_api_data`](https://cdcgov.github.io/Rnssp/reference/get_api_data.md)

## Examples

``` r
if (FALSE) { # \dontrun{
myProfile <- create_profile("", "")
url <- "http://httpbin.org/image/png"

api_data_graph <- get_api_graph(url)

names(api_data_graph)
img <- png::readPNG(api_data_graph$graph)
grid::grid.raster(img)
} # }
```
