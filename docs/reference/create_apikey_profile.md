# Create a profile with an API key

A wrapper to the `new` method defined in the
[`Apikey`](https://cdcgov.github.io/Rnssp/reference/Apikey.md) class.

## Usage

``` r
create_apikey_profile(
  api_key = getPass::getPass("Enter/Paste an API key: "),
  key_name = "API-KEY"
)
```

## Arguments

- api_key:

  a string for API key

- key_name:

  a string for an API Key name. Default is `API-KEY`

## Value

An object of class `Apikey`

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a profile with API key named "API-KEY"
myProfile <- create_apikey_profile()
myProfile

# Create a profile with an API key named "key"
myProfile2 <- create_apikey_profile(key_name = "key")
myProfile2
} # }
```
