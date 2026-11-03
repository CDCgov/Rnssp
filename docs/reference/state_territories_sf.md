# US States and Territories Shapefile

Useful `sf` object shapefile for US States and Territories.

## Usage

``` r
state_territories_sf
```

## Format

An object of class `sf` (inherits from `tbl_df`, `tbl`, `data.frame`)
with 56 rows and 4 columns.

## Details

    Rows: 56
    Columns: 4
    $ STATEFP  <chr> "01", "02", "04", "05", "06", "08", "09", "10", "11", "12", "13", "15", "16"…
    $ STUSPS   <chr> "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID"…
    $ NAME     <chr> "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Conne…
    $ geometry <MULTIPOLYGON [°]> MULTIPOLYGON (((-85.00065 3..., MULTIPOLYGON (((-113.0281 2...,…

## Examples

``` r
library(Rnssp)
data("state_territories_sf")

plot(state_territories_sf$geometry)
```
