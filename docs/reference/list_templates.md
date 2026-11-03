# List Available Report Templates

List available NSSP report templates from the Rnssp-rmd-templates Github
repository.

## Usage

``` r
list_templates(as.table = FALSE)
```

## Arguments

- as.table:

  a logical, if TRUE, a data frame is returned. Otherwise, a vector is
  returned (Default is FALSE).

## Value

A data frame or a vector

## See also

<https://github.com/CDCgov/Rnssp-rmd-templates>

## Examples

``` r
list_templates()
list_templates(as.table = TRUE)
```
