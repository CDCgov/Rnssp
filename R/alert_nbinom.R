#' Negative binomial regression model for weekly counts
#'
#' Negative binomial model helper function for monitoring weekly
#' count time series with seasonality
#'
#' @param df A data frame, data frame extension (e.g. a tibble),
#'     or a lazy data frame
#' @param t Name of the column of type Date containing the dates
#' @param y Name of the column of type Numeric containing counts
#' @param baseline_end Object of type Date defining the end of the
#'     baseline/training period
#' @param include_time Logical indicating whether or not to include time term
#'     in regression model
#'
#' @return A data frame.
#'
#' @keywords internal
#'
nb_model <- function(df, t, y, baseline_end, include_time) {

  t <- enquo(t)
  y <- enquo(y)

  df <- df %>%
    mutate(
      t  = as.Date(!!t),
      y  = as.numeric(!!y)
    )

  time_included <- TRUE

  input_data <- df %>%
    mutate(
      obs = row_number(),
      cos = cos((2 * pi * obs) / 52.18),
      sin = sin((2 * pi * obs) / 52.18),
      split = ifelse(t <= baseline_end, "Baseline Period", "Prediction Period")
    )

  baseline_data <- input_data %>%
    filter(split == "Baseline Period")

  predict_data <- input_data %>%
    filter(split == "Prediction Period")

  baseline_model <- MASS::glm.nb(y ~ obs + cos + sin, data = baseline_data)

  inv_link <- baseline_model$family$linkinv
  df_residual <- baseline_model$df.residual
  theta <- baseline_model$theta

  baseline_fit <- bind_cols(
    baseline_data,
    predict(baseline_model, simulate_pi = FALSE, se.fit = TRUE) %>%
      as.data.frame() %>%
      dplyr::select(fit_link = fit, se_link = se.fit) %>%
      mutate(
        estimate = inv_link(fit_link),
        upper_ci = inv_link(fit_link + qt(1 - 0.05, df_residual) * se_link),
        threshold = qnbinom(1 - 0.05, mu = upper_ci, size = theta)
      )
  )

  predict_fit <- bind_cols(
    predict_data,
    predict(baseline_model, newdata = predict_data, simulate_pi = FALSE,
            se.fit = TRUE, type = "link") %>%
      as.data.frame() %>%
      dplyr::select(fit_link = fit, se_link = se.fit) %>%
      mutate(
        estimate = inv_link(fit_link),
        upper_ci = inv_link(fit_link + qt(1 - 0.05, df_residual) * se_link),
        threshold = qnbinom(1 - 0.05, mu = upper_ci, size = theta)
      )
  )

  if (!include_time) {

    time_included <- FALSE

    baseline_model <- MASS::glm.nb(y ~ cos + sin, data = baseline_data)

    inv_link <- baseline_model$family$linkinv
    df_residual <- baseline_model$df.residual
    theta <- baseline_model$theta

    baseline_fit <- bind_cols(
      baseline_data,
      predict(baseline_model, simulate_pi = FALSE, se.fit = TRUE) %>%
        as.data.frame() %>%
        dplyr::select(fit_link = fit, se_link = se.fit) %>%
        mutate(
          estimate = inv_link(fit_link),
          upper_ci = inv_link(fit_link + qt(1 - 0.05, df_residual) * se_link),
          threshold = qnbinom(1 - 0.05, mu = upper_ci, size = theta)
        )
    )

    predict_fit <- bind_cols(
      predict_data,
      predict(baseline_model, newdata = predict_data, simulate_pi = FALSE,
              se.fit = TRUE, type = "link") %>%
        as.data.frame() %>%
        dplyr::select(fit_link = fit, se_link = se.fit) %>%
        mutate(
          estimate = inv_link(fit_link),
          upper_ci = inv_link(fit_link + qt(1 - 0.05, df_residual) * se_link),
          threshold = qnbinom(1 - 0.05, mu = upper_ci, size = theta)
        )
    )

  }

  bind_rows(baseline_fit, predict_fit) %>%
    arrange(!!t) %>%
    `rownames<-`( NULL ) %>%
    mutate(
      split = factor(split, levels = c("Baseline Period", "Prediction Period")),
      alarm = ifelse(!!y > threshold, TRUE, FALSE),
      time_term = time_included
    ) %>%
    select(-(t:sin), -fit_link, -se_link, -upper_ci)

}


#' Negative binomial detection algorithm for weekly counts
#'
#' The negative binomial regression algorithm fits a negative binomial regression
#' model with a time term and order 1 Fourier terms to a baseline period that
#' spans 2 or more years. Inclusion of Fourier terms in the model is intended
#' to account for seasonality common in multi-year weekly time series of counts.
#' Order 1 sine and cosine terms are included to account for annual seasonality
#' that is common to syndromes and diseases such as influenza, RSV, and norovirus.
#' Each baseline model is used to make weekly forecasts for all weeks following
#' the baseline period. One-sided upper 95% prediction interval bounds are
#' computed for each week in the prediction period. Alarms are signaled for
#' any week during for which weekly counts fall above the upper bound of
#' the prediction interval.
#'
#' @param df A data frame, data frame extension (e.g. a tibble), or
#'     a lazy data frame.
#' @param t Name of the column of type Date containing the dates
#' @param y Name of the column of type Numeric containing counts
#' @param baseline_end Object of type Date defining the end of the
#'     baseline/training period
#' @param include_time Logical indicating whether or not to include time term
#'     in regression model (default is \code{TRUE})
#'
#' @return A data frame with model estimates, upper prediction interval bounds,
#'     a binary alarm indicator field, and a binary indicator field of
#'     whether or not a time term was included.
#'
#' @export
#'
#' @examples
#' # Example 1
#'
#' df <- data.frame(
#'   date = seq(as.Date("2014-01-05"), as.Date("2022-02-05"), "weeks"),
#'   count = rpois(length(seq(as.Date("2014-01-05"), as.Date("2022-02-05"), "weeks")), 25)
#' )
#'
#' head(df)
#'
#' df_nbinom <- alert_nbinom(df, baseline_end = as.Date("2020-03-01"))
#'
#' head(df_nbinom)
#'
#'
#' # Example 2
#' library(ggplot2)
#'
#' simulated_ts <- simulated_data
#'
#' ## Time series with seasonality, moderate counts
#' ts <- subset(simulated_ts, id == "Scenario #1")
#'
#' head(ts)
#'
#' df_nbinom <- alert_nbinom(ts, t = date, y = cases,
#'                        baseline_end = as.Date("2021-12-26"))
#'
#' head(df_nbinom)
#'
#'
#' ## Visualize alert
#' df_nbinom %>%
#'  ggplot() +
#'  theme_classic() +
#'  geom_line(aes(x = date, y = cases), linewidth = 0.3) +
#'  geom_line(aes(x = date, y = estimate), color = "blue", linewidth = 0.3) +
#'  geom_line(aes(x = date, y = threshold), color = "red", linewidth = 0.3,
#'             linetype = "dashed") +
#'  geom_point(data = subset(df_nbinom, alarm), aes(x = date, y = cases),
#'             color = "red", shape = 21, size = 2.5) +
#'  scale_y_continuous(
#'    limits = c(0, 80),
#'    expand = c(0, 0),
#'    name = "Weekly Count"
#'  ) +
#'  scale_x_date(
#'    breaks = seq.Date(from = min(ts$date), to = max(ts$date), by = "4 month"),
#'    name = "MMWR Week Date"
#'  ) +
#'  theme(
#'    axis.text.x = element_text(angle = 90, vjust = 0.5),
#'    axis.ticks.length = unit(0.25, "cm")
#'  ) +
#'  labs(
#'    title = "Negative Binomial Regression Algorithm Results for Simulated Time Series #1",
#'    subtitle = "Annual seasonality with moderate counts"
#'  )
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
#' df_nbinom <- alert_nbinom(df, baseline_end = as.Date("2020-03-01"))
#'
#'
#' ### Visualize alert
#' df_nbinom %>%
#'   ggplot() +
#'   geom_line(aes(x = date, y = count), linewidth = 0.3) +
#'   geom_line(aes(x = date, y = estimate), color = "blue", linewidth = 0.3) +
#'   geom_line(aes(x = date, y = threshold), color = "red", linewidth = 0.3,
#'             linetype = "dashed") +
#'   geom_point(data = subset(df_nbinom, alarm),
#'              aes(x = date, y = count), color = "red", size = 0.7) +
#'   theme_classic() +
#'   scale_x_date(
#'     date_breaks = "6 month",
#'     date_labels = "%b-%Y"
#'   ) +
#'   scale_y_continuous(
#'     breaks = scales::pretty_breaks(n = 5),
#'     labels = scales::comma
#'   ) +
#'   labs(
#'     x = "Date",
#'     y = "Weekly ED Encounters",
#'     subtitle = "Negative binomial regression algorithm with order-1 Fourier terms"
#'   ) +
#'   geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dotted", linewidth = 0.5) +
#'   annotate(
#'     geom = "text",
#'     x = as.Date("2020-03-01") - 270,
#'     y = Inf, label = "End of baseline\nMarch 1, 2020",
#'     family = "Candara", vjust = 1.7, size = 3
#'   )
#'
#' }
#'
alert_nbinom <- function(df, t = date, y = count, include_time = TRUE, baseline_end) {

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
    cli::cli_abort("Error in {.fn alert_nbinom}: baseline length must be
                   greater than or equal to 2 years")
  }

  baseline <- df %>%
    filter(!!t <= baseline_end)

  baseline_dates <- baseline %>%
    pull(!!t)

  if (length(unique(seq.Date(min(baseline_dates), max(baseline_dates),
                             by = "1 week"))) != baseline_n_wks) {
    cli::cli_abort("Error in {.fn alert_nbinom}: not all weeks in intended
                   baseline date range were found")
  }

  # Check that time series observations are non-negative integer counts
  ts_obs <- df %>%
    pull(!!y)

  ts_obs_int <- df %>%
    pull(!!y) %>%
    as.integer()

  if (!all(ts_obs == ts_obs_int) | !(all(ts_obs >= 0))) {
    cli::cli_abort("Error in {.fn alert_nbinom}: time series observations
                   must be non-negative integer counts")
  }

  # Check for grouping variables
  grouped_df <- is.grouped_df(df)

  if (grouped_df) {
    groups <- group_vars(df)

    df %>%
      nest(data_split = -all_of(groups)) %>%
      mutate(
        detection = map(.x = data_split, .f = nb_model, t = !!t, y = !!y,
                        baseline_end = baseline_end, include_time = include_time)
      ) %>%
      select(-data_split) %>%
      unnest(detection)

  } else {
    unique_dates <- df %>%
      pull(!!t) %>%
      unique()

    if (length(unique_dates) != nrow(df)) {
      cli::cli_abort("Error in {.fn alert_nbinom}: Number of unique dates does
                     not equal the number of rows.
                     Should your dataframe be grouped?")
    }

    df %>%
      nest(data_split = everything()) %>%
      mutate(
        detection = map(.x = data_split, .f = nb_model, t = !!t, y = !!y,
                        baseline_end = baseline_end, include_time = include_time)
      ) %>%
      select(-data_split) %>%
      unnest(detection)

  }
}
