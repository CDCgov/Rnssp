# A `Token` Class Representing a Token object

A `Token` object has a token string and an authentication type.

## Details

A `Token` object can get API data via an API URL.

## Super class

`NSSPAuth` -\> `NSSPToken`

## Methods

### Public methods

- [`NSSPToken$new()`](#method-NSSPToken-initialize)

- [`NSSPToken$get_api_response()`](#method-NSSPToken-get_api_response)

- [`NSSPToken$get_api_graph()`](#method-NSSPToken-get_api_graph)

- [`NSSPToken$clone()`](#method-NSSPToken-clone)

Inherited methods

- `NSSPAuth$get_api_data()`

------------------------------------------------------------------------

### `NSSPToken$new()`

Initializes a new Credentials object.

#### Usage

    NSSPToken$new(token, auth_type = "Bearer")

#### Arguments

- `token`:

  a string for token

- `auth_type`:

  type of HTTP authentication. Should be `Bearer` or `Basic`. Default is
  `Bearer`

#### Returns

An object of class `Token`

------------------------------------------------------------------------

### `NSSPToken$get_api_response()`

Get API response

#### Usage

    NSSPToken$get_api_response(url)

#### Arguments

- `url`:

  a character of API URL

#### Returns

An object of class `response`

#### Examples

    myProfile <- Token$new("abc1234567890")
    url <- "https://httpbin.org/json"
    api_response <- myProfile$get_api_response(url)

------------------------------------------------------------------------

### `NSSPToken$get_api_graph()`

Get API graph

#### Usage

    NSSPToken$get_api_graph(url, file_ext = ".png")

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

    myProfile <- Token$new("abc1234567890")
    url <- "https://httpbin.org/image/png"
    api_data_graph <- myProfile$get_api_graph(url)
    names(api_data_graph)
    img <- png::readPNG(api_data_graph$graph)
    grid::grid.raster(img)

------------------------------------------------------------------------

### `NSSPToken$clone()`

The objects of this class are cloneable with this method.

#### Usage

    NSSPToken$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
## ------------------------------------------------
## Method `NSSPToken$get_api_response()`
## ------------------------------------------------

if (FALSE) { # \dontrun{
myProfile <- Token$new("abc1234567890")
url <- "https://httpbin.org/json"
api_response <- myProfile$get_api_response(url)
} # }

## ------------------------------------------------
## Method `NSSPToken$get_api_graph()`
## ------------------------------------------------

if (FALSE) { # \dontrun{
myProfile <- Token$new("abc1234567890")
url <- "https://httpbin.org/image/png"
api_data_graph <- myProfile$get_api_graph(url)
names(api_data_graph)
img <- png::readPNG(api_data_graph$graph)
grid::grid.raster(img)
} # }
```
