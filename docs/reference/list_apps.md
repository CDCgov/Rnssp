# List Available Shinyapps

List available NSSP Shinyapps from the Rnssp-shiny-apps Github
repository.

## Usage

``` r
list_apps(as.table = FALSE)
```

## Arguments

- as.table:

  a logical, if TRUE, a data frame is returned. Otherwise, a vector is
  returned (Default is FALSE).

## Value

A data frame or a vector

## See also

<https://github.com/CDCgov/Rnssp-shiny-apps>

## Examples

``` r
list_apps()
list_apps(as.table = TRUE)
```
