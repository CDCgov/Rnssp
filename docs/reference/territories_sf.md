# US Territories Shapefile

Useful`sf` object shapefile for US Territories.

## Usage

``` r
territories_sf
```

## Format

An object of class `sf` (inherits from `data.frame`) with 9 rows and 3
columns.

## Details

    Rows: 9
    Columns: 3
    $ STATEFP  <chr> "999", "72", "78", "66", "60", "69", "888", "777", "666"
    $ STUSPS   <chr> "NYC", "PR", "VI", "GU", "AS", "RMI", "MP", "PW", "FSM"
    $ geometry <POINT> POINT (2800000 0), POINT (2800000 -250000), POINT (2800000 -5e+05), POINT (2800000 -750000), POINT (2800000 -1e+06), POINT (280000...

## Examples

``` r
library(Rnssp)
data("territories_sf")

plot(territories_sf$geometry)
```
