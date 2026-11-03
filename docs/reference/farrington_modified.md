# Modified Farrington Algorithm

Modified Farrington Algorithm

## Usage

``` r
farrington_modified(df, t = date, y = count, B = 4, g = 27, w = 3, p = 10)
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

- g:

  Number of guardband weeks to separate the test date from the baseline
  (default is 27)

- w:

  Half the number of weeks included in reference window, before and
  after each reference date (default is 3)

- p:

  Number of seasonal periods for each year in baseline

## Value

A tibble
