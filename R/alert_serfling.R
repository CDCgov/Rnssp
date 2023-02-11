#' Original Serfling method for weekly time series
#'
#' Serfling model helper function for monitoring weekly time series with
#' seasonality
#'
#' @param df A data frame, data frame extension (e.g. a tibble), or a
#'     lazy data frame
#' @param t Name of the column of type Date containing the dates
#' @param y Name of the column of type Numeric containing counts or
#'     percentages
#' @param baseline_end Object of type Date defining the end of the
#'     baseline/training period
#'
#' @return A data frame.
#'
#' @keywords internal
#'
serfling_model <- function(df, t, y, baseline_end) {

  t <- enquo(t)
  y <- enquo(y)

  input_data <- df %>%
    mutate(
      t  = as.Date(!!t),
      y  = as.numeric(!!y),
      obs = row_number(),
      cos = cos((2 * pi * obs) / 52.18),
      sin = sin((2 * pi * obs) / 52.18),
      split = ifelse(t <= baseline_end, "Baseline Period", "Prediction Period"),
      epidemic_period = ifelse(month(t) >= 10 | month(t) <= 5, TRUE, FALSE)
    )

  baseline_data <- input_data %>%
    filter(split == "Baseline Period") %>%
    filter(!epidemic_period)

  baseline_model <- lm(y ~ obs + cos + sin, data = baseline_data)
  predict(baseline_model, newdata = input_data, se.fit = TRUE,
          interval = "prediction", level = 0.90) %>%
    as.data.frame() %>%
    select(estimate = fit.fit, threshold = fit.upr) %>%
    bind_cols(input_data) %>%
    relocate(c(estimate, threshold), .after = cos) %>%
    select(-obs, -cos, -sin) %>%
    mutate(
      split = factor(split, levels = c("Baseline Period", "Prediction Period")),
      alarm = ifelse(!!y > threshold, TRUE, FALSE)
    )
}

#' Original Serfling method for weekly time series
#'
#' The original Serfling algorithm fits a linear regression model with
#' a time term and order 1 Fourier terms to a baseline period that ideally spans
#' 5 or more years. Inclusion of Fourier terms in the model is intended to
#' account for seasonality common in multi-year weekly time series. Order 1 sine
#' and cosine terms are included to account for annual seasonality that is
#' common to syndromes and diseases such as influenza, RSV, and norovirus. Each
#' baseline model is used to make weekly forecasts for all weeks following the
#' baseline period. One-sided upper 95% prediction interval bounds are computed
#' for each week in the prediction period. Alarms are signaled for any week
#' during for which weekly observations fall above the upper bound of the
#' prediction interval. This implementation follows the approach of the original
#' Serfling method in which weeks between October of the starting year of a
#' season and May of the ending year of a season are considered to be in the
#' epidemic period. Weeks in the epidemic period are removed from the baseline
#' prior to fitting the regression model.
#'
#' @param df A data frame, data frame extension (e.g. a tibble), or a
#'     lazy data frame.
#' @param t Name of the column of type Date containing the dates
#' @param y Name of the column of type Numeric containing observations
#' @param baseline_end Object of type Date defining the end of the
#'     baseline/training period
#'
#' @return A data frame with model estimates, upper prediction interval bounds,
#'     and a binary alarm indicator field
#'     was included
#'
#' @examples
#' # Example 1
#' df <- data.frame(
#'   date = seq(as.Date("2014-01-05"), as.Date("2022-02-05"), "weeks"),
#'   count = rpois(length(seq(as.Date("2014-01-05"), as.Date("2022-02-05"), "weeks")), 25)
#' )
#'
#' head(df)
#'
#' df_serfling <- alert_serfling(df, baseline_end = as.Date("2020-03-01"))
#'
#' head(df_serfling)
#'
#' # Example 2
#' library(ggplot2)
#'
#' simulated_ts <- simulated_data
#'
#' ## Time series with seasonality, moderate counts
#' ts1 <- subset(simulated_ts, id == "Scenario #1")
#'
#' head(ts1)
#'
#' df_serfling <- alert_serfling(ts1, t = date, y = cases,
#'                             baseline_end = as.Date("2021-12-26"))
#'
#' head(df_serfling)
#'
#' ### Visualize alert
#' df_serfling %>%
#'  ggplot() +
#'  theme_classic() +
#'  geom_line(aes(x = date, y = cases), linewidth = 0.3) +
#'  geom_line(aes(x = date, y = estimate), color = "blue", linewidth = 0.3) +
#'  geom_line(aes(x = date, y = threshold), color = "red", linewidth = 0.3,
#'             linetype = "dashed") +
#'  geom_point(data = subset(df_serfling, alarm), aes(x = date, y = cases),
#'             color = "red", shape = 21, size = 2.5) +
#'  scale_y_continuous(
#'    limits = c(0, 80),
#'    expand = c(0, 0),
#'    name = "Weekly Count"
#'  ) +
#'  scale_x_date(
#'    breaks = seq.Date(from = min(ts1$date), to = max(ts1$date), by = "4 month"),
#'    name = "MMWR Week Date"
#'  ) +
#'  theme(
#'    axis.text.x = element_text(angle = 90, vjust = 0.5),
#'    axis.ticks.length = unit(0.25, "cm")
#'  ) +
#'  labs(
#'    title = "Original Serfling Method Results for Simulated Time Series #1",
#'    subtitle = "Annual seasonality with moderate counts"
#'  )
#'
#'
#'
#' \dontrun{
#' # Example 3: Data from NSSP-ESSENCE, national counts for CDC Respiratory Synctial Virus v1
#'
#' library(Rnssp)
#' library(ggplot2)
#'
#' myProfile <- create_profile()
#'
#' url <- "https://essence2.syndromicsurveillance.org/nssp_essence/api/timeSeries?endDate=12
#' Feb2022&ccddCategory=cdc%20respiratory%20syncytial%20virus%20v1&percentParam=noPercent
#' &geographySystem=hospitaldhhsregion&datasource=va_hospdreg&detector=nodetectordetector
#' &startDate=29Dec2013&timeResolution=weekly&hasBeenE=1&medicalGroupingSystem=essencesyndromes
#' &userId=2362&aqtTarget=TimeSeries"
#'
#'
#' url <- url %>% gsub("\n", "", .)
#'
#' api_data <- get_api_data(url)
#'
#' df <- api_data$timeSeriesData
#'
#'
#' df_serfling <- alert_serfling(df, baseline_end = as.Date("2020-03-01"))
#'
#'
#' ### Visualize alert
#' df_serfling %>%
#'   ggplot() +
#'   theme_classic() +
#'   geom_line(aes(x = date, y = count), linewidth = 0.3) +
#'   geom_line(aes(x = date, y = estimate), color = "blue", linewidth = 0.3) +
#'   geom_line(aes(x = date, y = threshold), color = "red", linewidth = 0.3,
#'             linetype = "dashed") +
#'   geom_point(data = subset(df_serfling, alarm), aes(x = date, y = count),
#'              color = "red", shape = 21, size = 2.5) +
#'   scale_x_date(
#'     breaks = seq.Date(from = min(df_serfling$date),
#'                       to = max(df_serfling$date), by = "4 month"),
#'     name = "MMWR Week Date"
#'   ) +
#'   theme(
#'     axis.text.x = element_text(angle = 90, vjust = 0.5),
#'     axis.ticks.length = unit(0.25, "cm")
#'   ) +
#'   labs(
#'     title = "Original Serfling Method Results for Weekly ED Encounters",
#'     subtitle = "Annual seasonality with moderate counts"
#'   )
#'
#' }
#'
alert_serfling <- function(df, t = date, y = count, baseline_end) {

  t <- enquo(t)
  y <- enquo(y)

  df <- df %>%
    mutate(
      {{ t }} := as.Date(!!t),
      {{ y }} := as.numeric(!!y)
    )

  # Check baseline length and for sufficient historical data
  baseline_n_wks <- df %>%
    filter(!!t <= baseline_end) %>%
    pull(!!t) %>%
    unique() %>%
    length()

  baseline_n_yrs <- baseline_n_wks / 52

  if (baseline_n_yrs < 2) {
    cli::cli_abort("Error in {.fn alert_serfling}: baseline length must be
                   greater than or equal to 2 years")
  }

  baseline <- df %>%
    filter(!!t <= baseline_end)

  baseline_dates <- baseline %>%
    pull(!!t)

  if (length(unique(seq.Date(min(baseline_dates), max(baseline_dates),
                             by = "1 week"))) != baseline_n_wks) {
    cli::cli_abort("Error in {.fn alert_serfling}: not all weeks in intended
                   baseline date range were found")
  }

  # Check for grouping variables
  grouped_df <- is.grouped_df(df)

  if (grouped_df) {
    groups <- group_vars(df)

    df %>%
      nest(data_split = -all_of(groups)) %>%
      mutate(
        detection = map(.x = data_split, .f = serfling_model, t = !!t, y = !!y,
                        baseline_end = baseline_end)
      ) %>%
      select(-data_split) %>%
      unnest(detection)

  } else {
    unique_dates <- df %>%
      pull(!!t) %>%
      unique()

    if (length(unique_dates) != nrow(df)) {
      cli::cli_abort("Error in {.fn alert_serfling}: Number of unique dates does
                     not equal the number of rows. Should your dataframe be grouped?")
    }

    df %>%
      nest(data_split = everything()) %>%
      mutate(
        detection = map(.x = data_split, .f = serfling_model, t = !!t, y = !!y,
                        baseline_end = baseline_end)
        ) %>%
      select(-data_split) %>%
      unnest(detection)
  }
}
