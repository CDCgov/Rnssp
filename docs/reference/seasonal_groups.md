# Return 10-level seasonal factor vector

Return 10-level seasonal factor vector

## Usage

``` r
seasonal_groups(B = 4, g = 27, w = 3, p = 10, base_length, base_weeks)
```

## Arguments

- B:

  Number of years to include in baseline (default is 4)

- g:

  Number of guardband weeks to separate the test week from the baseline
  (default is 27)

- w:

  Half the number of weeks included in reference window, before and
  after each reference date (default is 3)

- p:

  Number of seasonal periods for each year in baseline (default is 10)

- base_length:

  Total number of weeks included in baseline

- base_weeks:

  Indices of baseline weeks

## Value

A factor vector
