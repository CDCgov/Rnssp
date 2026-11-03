# Negative binomial regression model for weekly counts

Negative binomial model helper function for monitoring weekly count time
series with seasonality

## Usage

``` r
nb_model(df, t, y, baseline_end, include_time)
```

## Arguments

- df:

  A data frame, data frame extension (e.g. a tibble), or a lazy data
  frame

- t:

  Name of the column of type Date containing the dates

- y:

  Name of the column of type Numeric containing counts

- baseline_end:

  Object of type Date defining the end of the baseline/training period

- include_time:

  Logical indicating whether or not to include time term in regression
  model

## Value

A data frame.
