# Adaptive Regression

Adaptive Regression helper function for Adaptive Multiple Regression.

## Usage

``` r
adaptive_regression(df, t, y, B, g)
```

## Arguments

- df:

  A data frame, data frame extension (e.g. a tibble), or a lazy data
  frame

- t:

  Name of the column of type Date containing the dates

- y:

  Name of the column of type Numeric containing counts or percentages

- B:

  Baseline parameter. The baseline length is the number of days to which
  each liner model is fit.

- g:

  Guardband parameter. The guardband length is the number of days
  separating the baseline from the current date in consideration for
  alerting.

## Value

A data frame.
