# Original Serfling method for weekly time series

Serfling model helper function for monitoring weekly time series with
seasonality

## Usage

``` r
serfling_model(df, t, y, baseline_end)
```

## Arguments

- df:

  A data frame, data frame extension (e.g. a tibble), or a lazy data
  frame

- t:

  Name of the column of type Date containing the dates

- y:

  Name of the column of type Numeric containing counts or percentages

- baseline_end:

  Object of type Date defining the end of the baseline/training period

## Value

A data frame.
