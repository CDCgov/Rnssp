#' Adaptive Regression
#'
#' Adaptive Regression helper function for Adaptive Multiple Regression.
#'
#' @param df A data frame, data frame extension (e.g. a tibble), or a
#'     lazy data frame
#' @param t Name of the column of type Date containing the dates
#' @param y Name of the column of type Numeric containing counts or percentages
#' @param B Baseline parameter. The baseline length is the number of days to
#'     which each liner model is fit.
#' @param g Guardband parameter. The guardband length is the number of days
#'     separating the baseline from the current date in consideration for alerting.
#'
#' @return A data frame.
#'
#' @keywords internal
#'
adaptive_regression <- function(df, t, y, B, g) {
  t <- enquo(t)
  y <- enquo(y)
  
  N <- nrow(df)
  
  # Populate algorithm parameters
  min_df <- 3
  min_baseline <- 11
  max_baseline <- B
  df_range <- 1:(B - min_df)
  
  ucl_alert <- round(qt(1 - 0.01, df = df_range), 5)
  ucl_warning <- round(qt(1 - 0.05, df = df_range), 5)
  
  # Bound standard error of regression
  min_sigma <- 0.01 / ucl_warning
  
  # Initialize result vectors
  test_stat <- rep(NA, N)
  p_val <- rep(NA, N)
  expected <- rep(NA, N)
  sigma <- rep(NA, N)
  r_sqrd_adj <- rep(NA, N)
  
  # Vector of dates
  dates <- df %>%
    pull(!!t)
  
  # Vector of observations
  y_obs <- df %>%
    pull(!!y)
  
  # Initialize baseline indices
  ndx_baseline <- 1:(min_baseline - 1)
  
  # Adaptive multiple regression loop
  for (i in (min_baseline + g + 1):N) {
    
    # Pad baseline until full baseline is obtained
    if (last(ndx_baseline) < max_baseline) {
      ndx_baseline <- c(0, ndx_baseline)
    }
    
    # Advance baseline for current iteration
    ndx_baseline <- ndx_baseline + 1
    
    # Indices for baseline and test date
    if (last(ndx_baseline) < max_baseline) {
      ndx_time <- 1:last(ndx_baseline)
      ndx_test <- last(ndx_baseline) + g + 1
    } else {
      ndx_time <- 1:B
      ndx_test <- (B + g + 1)
    }
    
    # Set number of degrees of freedom
    n_df <- length(ndx_baseline) - 8
    
    # Baseline and current data
    baseline_data <- df[ndx_baseline, ]
    
    B_length <- length(ndx_baseline)
    
    # Baseline observed values
    baseline_obs <- baseline_data %>%
      pull(!!y)
    
    # Form regression matrix
    X <- as.matrix(
      cbind(ndx_time,
            baseline_data[, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat")])
    )
    
    # Fit regression model with lm.fit() for efficiency
    lm_fit <- lm.fit(x = cbind(1, X), y = baseline_obs)
    
    # Extract model components
    beta <- lm_fit$coefficients
    res <- lm_fit$residuals
    mse <- (1 / (n_df)) * sum(res^2)
    
    # Compute adjusted R-squared value
    fit_vals <- lm_fit$fitted.values
    mss <- sum((fit_vals - mean(fit_vals))^2)
    rss <- sum(res^2)
    r2 <- (mss / (rss + mss))
    r2_adj <- 1 - (1 - r2) * ((nrow(X) - 1) / lm_fit$df.residual)
    r_sqrd_adj[i] <- if_else(is.nan(r2_adj), 0, r2_adj)
    
    # Calculate bounded standard error of regression with derived formula for efficiency
    sigma[i] <- max(
      sqrt(mse) * sqrt(((B_length + 7) * (B_length - 4)) /
                         (B_length * (B_length - 7))), min_sigma[n_df]
    )
    
    # Day of week for test date
    dow_test <- as.numeric(format(dates[i], "%u"))
    
    # Calculate forecast on test date
    expected[i] <- if (dow_test < 7) {
      max(0, beta[[1]] + ndx_test * beta[[2]] + beta[[dow_test + 2]])
    } else {
      max(0, beta[[1]] + ndx_test * beta[[2]])
    }
    
    # Calculate test statistic
    test_stat[i] <- (y_obs[i] - expected[i]) / sigma[i]
    
    # Calculate p-value
    p_val[i] <- 1 - pt(test_stat[i], df = n_df)
  }
  
  tibble::tibble(
    baseline_expected = expected,
    test_statistic = test_stat,
    p.value = p_val,
    sigma = sigma,
    adjusted_r_squared = r_sqrd_adj
  )
}

#' Adaptive Multiple Regression
#'
#' The adaptive multiple regression algorithm fits a linear model to a baseline
#' of counts or percentages of length B, and forecasts a predicted value g + 1
#' days later (guard-band). This value is compared to the current observed value
#' and divided by the standard error of prediction in the test-statistic.
#' The model includes terms to account for linear trends and day-of-week effects.
#' Note that this implementation does NOT account for federal holidays as in the
#' Regression 1.2 algorithm in ESSENCE. An alert (red value) is signaled if
#' the statistical test (student's t-test) applied to the test statistic yields
#' a p-value less than 0.01. If the p-value is greater than or equal to 0.01
#' and strictly less than 0.05, a warning (yellow value) is signaled.
#' Blue values are returned if an alert or warning does not occur.
#' Grey values represent instances where anomaly detection did not apply
#' (i.e., observations for which baseline data were unavailable).
#'
#' @param df A data frame, data frame extension (e.g. a tibble), or a
#'     lazy data frame.
#' @param t Name of the column of type Date containing the dates
#' @param y Name of the column of type Numeric containing counts or percentages
#' @param B Baseline parameter. The baseline length is the number of days to
#'     which each liner model is fit (default is 28)
#' @param g Guardband parameter. The guardband length is the number of days
#'     separating the baseline from the current date in consideration for
#'     alerting (default is 2)
#'
#' @return A data frame with test statistic, p.value, and alert indicator
#' @references
#' \itemize{
#'     \item \href{https://www.cambridge.org/core/books/introduction-to-statistical-methods-for-biosurveillance/370B0E8DC0C1DE12FCE2027CE653F332}{Statistical Methods for Biosurveillance, Fricker (2013)}
#'     \item \href{https://www.jhuapl.edu/content/techdigest/pdf/V27-N04/27-04-BurkomDevelopments.pdf}{Developments in the Roles, Features, and Evaluation of Alerting Algorithms for Disease Outbreak Monitoring}
#' }
#'
#'
#' @export
#'
#' @examples
#'
#' # Example 1
#' df <- data.frame(
#'   date = seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 1),
#'   count = floor(runif(366, min = 0, max = 101))
#' )
#'
#' head(df)
#'
#' df_regression <- alert_regression(df)
#'
#' head(df_regression)
#'
#' df <- data.frame(
#'   Date = seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 1),
#'   percent = runif(366)
#' )
#'
#' head(df)
#'
#' df_regression <- alert_regression(df, t = Date, y = percent)
#'
#' head(df_regression)
#'
#' \dontrun{
#' # Example 3: Data from NSSP-ESSENCE
#' library(ggplot2)
#'
#' myProfile <- create_profile()
#'
#' url <- "https://essence2.syndromicsurveillance.org/nssp_essence/api/timeSeries?
#' endDate=20Nov20&ccddCategory=cli%20cc%20with%20cli%20dd%20and%20coronavirus%20dd%20v2
#' &percentParam=ccddCategory&geographySystem=hospitaldhhsregion&datasource=va_hospdreg
#' &detector=probrepswitch&startDate=22Aug20&timeResolution=daily&hasBeenE=1
#' &medicalGroupingSystem=essencesyndromes&userId=2362&aqtTarget=TimeSeries&stratVal=
#' &multiStratVal=geography&graphOnly=true&numSeries=0&graphOptions=multipleSmall
#' &seriesPerYear=false&nonZeroComposite=false&removeZeroSeries=true&startMonth=January
#' &stratVal=&multiStratVal=geography&graphOnly=true&numSeries=0&graphOptions=multipleSmall
#' &seriesPerYear=false&startMonth=January&nonZeroComposite=false"
#'
#' url <- url %>% gsub("\n", "", .)
#'
#' api_data <- get_api_data(url)
#'
#' df <- api_data$timeSeriesData
#'
#' df_regression <- df %>%
#'   group_by(hospitaldhhsregion_display) %>%
#'   alert_regression(t = date, y = dataCount)
#'
#' # Visualize alert for HHS Region 4
#' df_regression_region <- df_regression %>%
#'   filter(hospitaldhhsregion_display == "Region 4")
#'
#' df_regression_region %>%
#'   ggplot() +
#'   geom_line(aes(x = date, y = dataCount), color = "grey70") +
#'   geom_line(
#'     data = subset(df_regression_region, alert != "grey"),
#'     aes(x = date, y = dataCount), color = "navy"
#'   ) +
#'   geom_point(
#'     data = subset(df_regression_region, alert == "blue"),
#'     aes(x = date, y = dataCount), color = "navy"
#'   ) +
#'   geom_point(
#'     data = subset(df_regression_region, alert == "yellow"),
#'     aes(x = date, y = dataCount), color = "yellow"
#'   ) +
#'   geom_point(
#'     data = subset(df_regression_region, alert == "red"),
#'     aes(x = date, y = dataCount), color = "red"
#'   ) +
#'   theme_bw() +
#'   labs(
#'     x = "Date",
#'     y = "Count"
#'   )
#' }
#'
alert_regression <- function(df, t = date, y = count, B = 28, g = 2) {
  
  # Check baseline length argument
  if (B < 7) {
    cli::cli_abort("Error in {.fn alert_regression}: baseline length argument
                   {.var B} must be greater than or equal to 7")
  }
  
  if (B %% 7 != 0) {
    cli::cli_abort("Error in {.fn alert_regression}: baseline length argument
                   {.var B} must be a multiple of 7")
  }
  
  # Check guardband length argument
  if (g < 0) {
    cli::cli_abort("Error in {.fn alert_regression}: guardband length argument
                   {.var g} cannot be negative")
  }
  
  # Check for sufficient baseline data
  if (nrow(df) < B + g + 1) {
    cli::cli_abort("Error in {.fn alert_regression}: not enough historical data")
  }
  
  # Check for grouping variables
  grouped_df <- is.grouped_df(df)
  
  t <- enquo(t)
  y <- enquo(y)
  
  base_tbl <- df %>%
    mutate(
      {{ t }} := as.Date(!!t),
      dow = weekdays(!!t, abbreviate = TRUE),
      dummy = 1
    ) %>%
    pivot_wider(names_from = dow, values_from = dummy, values_fill = 0)
  
  if (grouped_df) {
    groups <- group_vars(base_tbl)
    
    base_tbl %>%
      nest(data_split = -all_of(groups)) %>%
      mutate(anomalies = map(
        .x = data_split,
        .f = adaptive_regression, t = !!t, y = !!y, B = B, g = g)
      ) %>%
      unnest(c(data_split, anomalies)) %>%
      mutate(
        alert = case_when(
          p.value < 0.01 ~ "red",
          p.value >= 0.01 & p.value < 0.05 ~ "yellow",
          p.value >= 0.05 ~ "blue",
          TRUE ~ "grey"
        )
      ) %>%
      select(-c(Mon, Tue, Wed, Thu, Fri, Sat, Sun))
    
    reg_initial <- base_tbl %>%
      nest(data_split = -all_of(groups)) %>%
      mutate(anomalies = map(
        .x = data_split,
        .f = adaptive_regression2, t = !!t, y = !!y, B = B, g = g)
      ) %>%
      unnest(c(data_split, anomalies)) %>%
      mutate(
        alert = case_when(
          p.value < 0.01 ~ "red",
          p.value >= 0.01 & p.value < 0.05 ~ "yellow",
          p.value >= 0.05 ~ "blue",
          TRUE ~ "grey"
        )
      ) %>%
      select(-c(Mon, Tue, Wed, Thu, Fri, Sat, Sun))
    
    ds_check1 <- reg_initial %>%
      group_by(across(all_of(groups))) %>%
      mutate(
        dow = weekdays(!!t, abbreviate = TRUE), 
        ma7 = zoo::rollmean(!!y, k = 7, align = "right", fill = NA), 
        lt_ma7 = if_else(!!y < ma7, TRUE, FALSE), 
        remove_alert = if_else(alert != "blue" & lt_ma7, TRUE, FALSE)
      )
    
    ds_check2 <- ds_check1 %>%
      filter(dow %in% c("Sat", "Sun")) %>%
      arrange(get(groups_t), dow, !!t) %>%
      group_by(across(all_of(groups)), dow) %>%
      mutate(
        wday_ma7 = zoo::rollmean(!!y, k = 7, align = "right", fill = NA), 
        lt_wday_ma7 = if_else(!!y < wday_ma7, TRUE, FALSE), 
        remove_wday_alert = case_when(
          alert != "blue" & remove_alert & lt_wday_ma7 ~ TRUE, 
          alert != "blue" & remove_alert & !lt_wday_ma7 ~ FALSE,
          TRUE ~ FALSE
        )
      ) %>%
      ungroup() %>%
      select(
        all_of(groups), 
        !!t,
        wday_ma7, 
        remove_wday_alert
      )
    
    ds_check1 %>%
      left_join(ds_check2, by = c(groups, quo_name(t))) %>%
      mutate(
        alert_status_final = case_when(
          remove_alert & (!dow %in% c("Sat", "Sun")) ~ "grey",
          remove_alert & remove_wday_alert ~ "grey", 
          remove_alert & !remove_wday_alert ~ alert,
          TRUE ~ alert
        )
      ) %>%
      select(-(dow:remove_wday_alert)) %>%
      select(-alert) %>%
      rename(alert = alert_status_final)
    
  } else {
    unique_dates <- base_tbl %>%
      pull(!!t) %>%
      unique()
    
    if (length(unique_dates) != nrow(base_tbl)) {
      cli::cli_abort("Error in {.fn alert_regression}: Number of unique dates does
                     not equal the number of rows. Should your dataframe be grouped?")
    }
    
    reg_initial <- base_tbl %>%
      nest(data_split = everything()) %>%
      mutate(anomalies = map(
        .x = data_split,
        .f = adaptive_regression, t = !!t, y = !!y, B = B, g = g)
      ) %>%
      unnest(c(data_split, anomalies)) %>%
      mutate(
        alert = case_when(
          p.value < 0.01 ~ "red",
          p.value >= 0.01 & p.value < 0.05 ~ "yellow",
          p.value >= 0.05 ~ "blue",
          TRUE ~ "grey"
        )
      ) %>%
      select(-c(Mon, Tue, Wed, Thu, Fri, Sat, Sun))
    
    ds_check1 <- reg_initial %>%
      mutate(
        dow = weekdays(!!t, abbreviate = TRUE), 
        ma7 = zoo::rollmean(!!y, k = 7, align = "right", fill = NA), 
        lt_ma7 = if_else(!!y < ma7, TRUE, FALSE), 
        remove_alert = if_else(alert != "blue" & lt_ma7, TRUE, FALSE)
      )
    
    ds_check2 <- ds_check1 %>%
      filter(dow %in% c("Sat", "Sun")) %>%
      group_by(dow) %>%
      arrange(dow, !!t) %>%
      mutate(
        wday_ma7 = zoo::rollmean(!!y, k = 7, align = "right", fill = NA), 
        lt_wday_ma7 = if_else(!!y < wday_ma7, TRUE, FALSE), 
        remove_wday_alert = case_when(
          alert != "blue" & remove_alert & lt_wday_ma7 ~ TRUE, 
          alert != "blue" & remove_alert & !lt_wday_ma7 ~ FALSE,
          TRUE ~ FALSE
        )
      ) %>%
      ungroup() %>%
      select(
        !!t,
        wday_ma7, 
        remove_wday_alert
      )
    
    ds_check1 %>%
      left_join(ds_check2, by = quo_name(t)) %>%
      mutate(
        alert_status_final = case_when(
          remove_alert & (!dow %in% c("Sat", "Sun")) ~ "blue",
          remove_alert & remove_wday_alert ~ "blue", 
          remove_alert & !remove_wday_alert ~ alert,
          TRUE ~ alert
        )
      ) %>%
      select(-(dow:remove_wday_alert)) %>%
      select(-alert) %>%
      rename(alert = alert_status_final)
    
  }
}
