# A `Credentials` Class Representing a Credentials object

A `Credentials` object has a username and a password.

## Details

A `Credentials` object can get API data via an API URL.

## Super class

`NSSPAuth` -\> `NSSPCredentials`

## Methods

### Public methods

- [`NSSPCredentials$new()`](#method-NSSPCredentials-initialize)

- [`NSSPCredentials$get_api_response()`](#method-NSSPCredentials-get_api_response)

- [`NSSPCredentials$get_api_graph()`](#method-NSSPCredentials-get_api_graph)

- [`NSSPCredentials$clone()`](#method-NSSPCredentials-clone)

Inherited methods

- `NSSPAuth$get_api_data()`

------------------------------------------------------------------------

### `NSSPCredentials$new()`

Initializes a new Credentials object.

#### Usage

    NSSPCredentials$new(username = NULL, password = NULL)

#### Arguments

- `username`:

  a string for username

- `password`:

  a string for password

#### Returns

A new `Credentials` object

------------------------------------------------------------------------

### `NSSPCredentials$get_api_response()`

Get API response

#### Usage

    NSSPCredentials$get_api_response(url)

#### Arguments

- `url`:

  a character of API URL

#### Returns

An object of class `response`

#### Examples

    myProfile <- Credentials$new(askme("Enter my username: "), askme())
    url <- "https://httpbin.org/json"
    api_response <- myProfile$get_api_response(url)

------------------------------------------------------------------------

### `NSSPCredentials$get_api_graph()`

Get API graph

#### Usage

    NSSPCredentials$get_api_graph(url, file_ext = ".png")

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

    myProfile <- Credentials$new(askme("Enter my username: "), askme())
    url <- "<url>"
    api_data_graph <- myProfile$get_api_graph(url)
    names(api_data_graph)
    img <- png::readPNG(api_data_graph$graph)
    grid::grid.raster(img)

------------------------------------------------------------------------

### `NSSPCredentials$clone()`

The objects of this class are cloneable with this method.

#### Usage

    NSSPCredentials$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
## ------------------------------------------------
## Method `NSSPCredentials$get_api_response()`
## ------------------------------------------------

if (FALSE) { # \dontrun{
myProfile <- Credentials$new(askme("Enter my username: "), askme())
url <- "https://httpbin.org/json"
api_response <- myProfile$get_api_response(url)
} # }

## ------------------------------------------------
## Method `NSSPCredentials$get_api_graph()`
## ------------------------------------------------

if (FALSE) { # \dontrun{
myProfile <- Credentials$new(askme("Enter my username: "), askme())
url <- "<url>"
api_data_graph <- myProfile$get_api_graph(url)
names(api_data_graph)
img <- png::readPNG(api_data_graph$graph)
grid::grid.raster(img)
} # }
```
