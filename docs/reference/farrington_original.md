# Original Farrington Algorithm

Original Farrington Algorithm

## Usage

``` r
farrington_original(df, t = date, y = count, B = 4, w = 3)
```

## Arguments

- df:

  Nested dataframe

- t:

  A column containing date values

- y:

  A column containing time series counts

- B:

  Number of years to include in baseline (default is 4)

- w:

  Half the number of weeks included in reference window, before and
  after each reference date (default is 3)

## Value

A tibble
