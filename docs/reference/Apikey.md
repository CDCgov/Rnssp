# An `Apikey` Class Representing an Apikey object

A `Apikey` object has an api_key string and a key name string.

## Details

An `Apikey` object can get API data via an API URL.

## Super class

`NSSPAuth` -\> `NSSPApikey`

## Methods

### Public methods

- [`NSSPApikey$new()`](#method-NSSPApikey-initialize)

- [`NSSPApikey$get_api_response()`](#method-NSSPApikey-get_api_response)

- [`NSSPApikey$get_api_graph()`](#method-NSSPApikey-get_api_graph)

- [`NSSPApikey$clone()`](#method-NSSPApikey-clone)

Inherited methods

- `NSSPAuth$get_api_data()`

------------------------------------------------------------------------

### `NSSPApikey$new()`

Initializes a new Credentials object.

#### Usage

    NSSPApikey$new(api_key, key_name = "API-KEY")

#### Arguments

- `api_key`:

  a string for API key

- `key_name`:

  a string for an API Key name. Default is `API-KEY`

#### Returns

An object of class `Apikey`

------------------------------------------------------------------------

### `NSSPApikey$get_api_response()`

Get API response

#### Usage

    NSSPApikey$get_api_response(url)

#### Arguments

- `url`:

  a character of API URL

#### Returns

An object of class `response`

#### Examples

    myProfile <- Apikey$new("abc1234567890")
    url <- "https://httpbin.org/json"
    api_response <- myProfile$get_api_response(url)

------------------------------------------------------------------------

### `NSSPApikey$get_api_graph()`

Get API graph

#### Usage

    NSSPApikey$get_api_graph(url, file_ext = ".png")

#### Arguments

- `url`:

  a character of API URL

- `file_ext`:

  a non-empty character vector giving the file extension. Default is
  `.png`.

#### Returns

A list containing an api_response object and a path to a time series
graph in .png format

#### Examples

    myProfile <- Apikey$new("abc1234567890")
    url <- "https://httpbin.org/image/png"
    api_data_graph <- myProfile$get_api_graph(url)
    names(api_data_graph)
    img <- png::readPNG(api_data_graph$graph)
    grid::grid.raster(img)

------------------------------------------------------------------------

### `NSSPApikey$clone()`

The objects of this class are cloneable with this method.

#### Usage

    NSSPApikey$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
## ------------------------------------------------
## Method `NSSPApikey$get_api_response()`
## ------------------------------------------------

if (FALSE) { # \dontrun{
myProfile <- Apikey$new("abc1234567890")
url <- "https://httpbin.org/json"
api_response <- myProfile$get_api_response(url)
} # }

## ------------------------------------------------
## Method `NSSPApikey$get_api_graph()`
## ------------------------------------------------

if (FALSE) { # \dontrun{
myProfile <- Apikey$new("abc1234567890")
url <- "https://httpbin.org/image/png"
api_data_graph <- myProfile$get_api_graph(url)
names(api_data_graph)
img <- png::readPNG(api_data_graph$graph)
grid::grid.raster(img)
} # }
```
