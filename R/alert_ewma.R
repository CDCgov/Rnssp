#' Loop for EWMA
#'
#' Loop for EWMA and adjustment of outlying smoothed values
#'
#' @param df A data frame, data frame extension (e.g., a tibble), or a lazy data frame
#' @param t Name of the column of type Date containing the dates
#' @param y Numeric vector of counts or percentages
#' @param mu Numeric vector of baseline averages
#' @param B Baseline parameter. The baseline length is the number of days used to
#'     calculate rolling averages, standard deviations, and exponentially weighted
#'     moving averages. Defaults to 28 days to match ESSENCE implementation.
#' @param g Guardband parameter. The guardband length is the number of days separating
#'     the baseline from the current test date. Defaults to 2 days to match ESSENCE
#'     implementation.
#' @param w1 Smoothing coefficient for sensitivity to gradual events. Must be between
#'     0 and 1 and is recommended to be between 0.3 and 0.5 to account for gradual
#'     effects. Defaults to 0.4 to match ESSENCE implementation.
#' @param w2 Smoothed coefficient for sensitivity to sudden events. Must be between
#'     0 and 1 and is recommended to be above 0.7 to account for sudden events.
#'     Defaults to 0.9 to match ESSENCE implementation and approximate the C2 algorithm.
#' @keywords internal
#' @return A data frame with p-values and test statistics
#'

ewma_loop <- function(df, t, y, B, g, w1, w2) {
  t <- enquo(t)
  y <- enquo(y)
  
  N <- nrow(df)
  
  # Populate algorithm parameters
  # min_df <- 3
  # min_baseline <- 11
  min_baseline <- 7
  max_baseline <- B
  length_baseline <- min_baseline:max_baseline
  
  # Vector of observations
  y_initial <- df %>%
    pull(!!y)
  
  # Scaling for percentage time series with values < 1
  if (max(y_initial) < 1 & length(y_initial[y_initial > 0]) >= 1) {
    y <- y_initial / median(y_initial[y_initial > 0])
  } else if (sum(y_initial) == 0) {
    y <- y_initial
  } else {
    y <- y_initial
  }
  
  # Initialize result vectors
  expected <- rep(NA, N)
  z1 <- rep(NA, N)
  z2 <- rep(NA, N)
  sigma1 <- rep(NA, N)
  sigma2 <- rep(NA, N)
  test_stat1 <- rep(NA, N)
  test_stat2 <- rep(NA, N)
  pval1 <- rep(NA, N)
  pval2 <- rep(NA, N)
  
  z <- z1
  test_stat <- test_stat1
  p_val <- pval1
  
  # Initialize EWMA values
  z1[1] <- y[1]
  z2[1] <- y[1]
  
  for (i0 in 2:(min_baseline + g)) {
    z1[i0] <- w1 * y[i0] + (1 - w1) * z1[i0 - 1]
    z2[i0] <- w2 * y[i0] + (1 - w2) * z2[i0 - 1]
  }
  
  # Initialize baseline indices
  ndx_baseline <- 1:(min_baseline - 1)
  
  # EWMA loop
  for (i in (min_baseline + g + 1):N) {
    
    # Pad baseline until full baseline is obtained
    if (last(ndx_baseline) < max_baseline) {
      ndx_baseline <- c(0, ndx_baseline)
    }
    
    # Advance baseline for current iteration
    ndx_baseline <- ndx_baseline + 1
    
    # Set number of degrees of freedom
    n_df <- length(ndx_baseline) - 1
    
    # Baseline and current data
    y_baseline <- y[ndx_baseline]
    
    expected[i] <- mean(y_baseline)
    sigma <- sd(y_baseline)
    
    sigma_correction1 <- sqrt(
      (w1 / (2 - w1)) + (1 / length(ndx_baseline)) - 2 * (1 - w1)^(g + 1) *
        ((1 - (1 - w1)^length(ndx_baseline)) / length(ndx_baseline))
    )
    sigma_correction2 <- sqrt(
      (w2 / (2 - w2)) + (1 / length(ndx_baseline)) - 2 * (1 - w2)^(g + 1) *
        ((1 - (1 - w2)^length(ndx_baseline)) / length(ndx_baseline))
    )
    
    ucl_alert <- round(qt(1 - 0.01, df = n_df), 5)
    ucl_warning <- round(qt(1 - 0.05, df = n_df), 5)
    
    min_sigma1 <- (w1 / ucl_warning) * (0.01 + 0.05 * (1 - w1)^2)
    min_sigma2 <- (w2 / ucl_warning) * (0.01 + 0.05 * (1 - w2)^2)
    
    constant1 <- (0.1289 - (0.2414 - 0.1826 * (1 - w1)^4) *
                    log(10 * 0.05)) * (w1 / ucl_warning)
    constant2 <- (0.1289 - (0.2414 - 0.1826 * (1 - w2)^4) *
                    log(10 * 0.05)) * (w2 / ucl_warning)
    
    sigma1[i] <- max(min_sigma1, sigma * sigma_correction1 + constant1)
    sigma2[i] <- max(min_sigma2, sigma * sigma_correction2 + constant2)
    
    # EWMA values
    z1[i] <- w1 * y[i] + (1 - w1) * z1[i - 1]
    z2[i] <- w2 * y[i] + (1 - w2) * z2[i - 1]
    
    # Calculate test statistics
    test_stat1[i] <- (z1[i] - expected[i]) / sigma1[i]
    test_stat2[i] <- (z2[i] - expected[i]) / sigma2[i]
    
    if (abs(test_stat1[i]) > ucl_alert) {
      z1[i] <- expected[i] + sign(test_stat1[i]) * ucl_alert * sigma1[i]
    }
    
    if (abs(test_stat2[i]) > ucl_alert) {
      z2[i] <- expected[i] + sign(test_stat2[i]) * ucl_alert * sigma2[i]
    }
    
    # Compute p-values
    pval1[i] <- 1 - pt(test_stat1[i], df = n_df)
    pval2[i] <- 1 - pt(test_stat2[i], df = n_df)
    
    # Determine minimum p-value
    if (pval1[i] < pval2[i]) {
      p_val[i] <- pval1[i]
      test_stat[i] <- test_stat1[i]
      z[i] <- z1[i]
    } else {
      p_val[i] <- pval2[i]
      test_stat[i] <- test_stat2[i]
      z[i] <- z2[i]
    }
  }
  
  tibble::tibble(
    baseline_expected = expected,
    test_statistic = test_stat,
    p.value = p_val
  )
}

#' Exponentially Weighted Moving Average (EWMA)
#'
#' The EWMA compares a weighted average of the most recent visit counts
#' to a baseline expectation. For the weighted average to be tested, an exponential
#' weighting gives the most influence to the most recent observations.
#' This algorithm is appropriate for daily counts that do not have the
#' characteristic features modeled in the regression algorithm. It is more applicable
#' for Emergency Department data from certain hospital groups and for time series with
#' small counts (daily average below 10) because of the limited case definition or
#' chosen geographic region. An alert (red value) is signaled if the statistical test
#' (student's t-test) applied to the test statistic yields a p-value less than 0.01.
#' If the p-value is greater than or equal to 0.01 and strictly less than 0.05, a warning
#' (yellow value) is signaled. Blue values are returned if an alert or warning does not
#' occur. Grey values represent instances where anomaly detection did not apply
#' (i.e., observations for which baseline data were unavailable).
#'
#' @param df A data frame, data frame extension (e.g., a tibble), or a lazy data frame
#' @param t Name of the column of type Date containing the dates
#' @param y Name of the column of type Numeric containing counts or percentages
#' @param B Baseline parameter. The baseline length is the number of days used to
#'     calculate rolling averages, standard deviations, and exponentially weighted
#'     moving averages. Defaults to 28 days to match ESSENCE implementation.
#' @param g Guardband parameter. The guardband length is the number of days separating
#'     the baseline from the current test date. Defaults to 2 days to match ESSENCE
#'     implementation.
#' @param w1 Smoothing coefficient for sensitivity to gradual events. Must be between
#'     0 and 1 and is recommended to be between 0.3 and 0.5 to account for gradual
#'     effects. Defaults to 0.4 to match ESSENCE implementation.
#' @param w2 Smoothed coefficient for sensitivity to sudden events. Must be between
#'     0 and 1 and is recommended to be above 0.7 to account for sudden events.
#'     Defaults to 0.9 to match ESSENCE implementation and approximate the C2 algorithm.
#'
#' @return Original data frame with detection results.
#' @export
#'
#' @examples
#' # Example 1
#' df <- data.frame(
#'   date = seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 1),
#'   count = floor(runif(366, min = 0, max = 101))
#' )
#'
#' head(df)
#'
#' df_ewma <- alert_ewma(df)
#'
#' head(df_ewma)
#'
#' # Example 2
#' df <- data.frame(
#'   Date = seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 1),
#'   percent = runif(366)
#' )
#'
#' head(df)
#'
#' df_ewma <- alert_ewma(df, t = Date, y = percent)
#'
#' head(df_ewma)
#'
#'
#' \dontrun{
#' # Example 3: Data from NSSP-ESSENCE
#' library(Rnssp)
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
#' df_ewma <- df %>%
#'   group_by(hospitaldhhsregion_display) %>%
#'   alert_ewma(t = date, y = dataCount)
#'
#' # Visualize alert for HHS Region 4
#' df_ewma_region <- df_ewma %>%
#'   filter(hospitaldhhsregion_display == "Region 4")
#'
#' df_ewma_region %>%
#'   ggplot() +
#'   geom_line(aes(x = date, y = dataCount), color = "grey70") +
#'   geom_line(
#'     data = subset(df_ewma_region, alert != "grey"),
#'     aes(x = date, y = dataCount), color = "navy"
#'   ) +
#'   geom_point(
#'     data = subset(df_ewma_region, alert == "blue"),
#'     aes(x = date, y = dataCount), color = "navy"
#'   ) +
#'   geom_point(
#'     data = subset(df_ewma_region, alert == "yellow"),
#'     aes(x = date, y = dataCount), color = "yellow"
#'   ) +
#'   geom_point(
#'     data = subset(df_ewma_region, alert == "red"),
#'     aes(x = date, y = dataCount), color = "red"
#'   ) +
#'   theme_bw() +
#'   labs(
#'     x = "Date",
#'     y = "Count"
#'   )
#' }
#'
alert_ewma <- function(df, t = date, y = count,
                        B = 28, g = 2, w1 = 0.4, w2 = 0.9) {
  
  # Check baseline length argument
  if (B < 7) {
    cli::cli_abort("Error in {.fn alert_ewma}: baseline length argument {.var B}
                   must be greater than or equal to 7")
  }
  
  # Check guardband length argument
  if (g < 0) {
    cli::cli_abort("Error in {.fn alert_ewma}: guardband length argument {.var g}
                   cannot be negative")
  }
  
  # Check for sufficient baseline data
  if (nrow(df) < B + g + 1) {
    cli::cli_abort("Error in {.fn alert_ewma}: not enough historical data")
  }
  
  # Check for grouping variables
  grouped_df <- is.grouped_df(df)
  
  t <- enquo(t)
  y <- enquo(y)
  
  base_tbl <- df %>%
    mutate({{ t }} := as.Date(!!t))
  
  if (grouped_df) {
    groups <- group_vars(base_tbl)
    
    alert_tbl <- base_tbl %>%
      nest(data_split = -all_of(groups)) %>%
      mutate(
        anomalies = map(.x = data_split, .f = ewma_loop,
                        t = !!t, y = !!y, B = B, g = g, w1 = w1, w2 = w2)
      ) %>%
      unnest(c(data_split, anomalies)) %>%
      mutate(
        alert = case_when(
          p.value < 0.01 ~ "red",
          p.value >= 0.01 & p.value < 0.05 ~ "yellow",
          p.value >= 0.05 ~ "blue",
          TRUE ~ "grey"
        )
      )
    
    return(alert_tbl)
  } else {
    unique_dates <- base_tbl %>%
      pull(!!t) %>%
      unique()
    
    if (length(unique_dates) != nrow(base_tbl)) {
      cli::cli_abort("Error in {.fn alert_regression}: Number of unique dates
                     does not equal the number of rows.
                     Should your dataframe be grouped?")
    }
    
    alert_tbl <- base_tbl %>%
      nest(data_split = everything()) %>%
      mutate(
        anomalies = map(.x = data_split, .f = ewma_loop,
                        t = !!t, y = !!y, B = B, g = g, w1 = w1, w2 = w2)
      ) %>%
      unnest(c(data_split, anomalies)) %>%
      mutate(
        alert = case_when(
          p.value < 0.01 ~ "red",
          p.value >= 0.01 & p.value < 0.05 ~ "yellow",
          p.value >= 0.05 ~ "blue",
          TRUE ~ "grey"
        )
      )
  }
}
