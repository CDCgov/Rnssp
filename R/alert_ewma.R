#' Loop for EWMA
#'
#' Loop for EWMA and adjustment of outlying smoothed values
#'
#' @param y Numeric vector of counts or percentages
#'
#' @param mu Numeric vector of baseline averages
#'
#' @param sigma1 Adjusted standard deviation for baselines and
#'     first weighting for sensitivity to gradual events
#'
#' @param sigma2 Adjusted standard deviation for baselines and
#'     second weighting for sensitivity to sudden events
#'
#' @param w Vector of length 2 with smoothing coefficients for
#'     gradual and sudden events
#'
#' @param alert_thresh Alert threshold value
#'
#' @keywords internal
#'
#' @return List of output p-values, test statistics, and Exponentially
#'     Weighted Moving Averages (EWMAs)
#'

ewma_loop <- function(y, mu, sigma1, sigma2, B, g, w, alert_thresh) {
  z1 <- rep(0, length(y))
  z2 <- rep(0, length(y))
  test_stat1 <- rep(0, length(y))
  test_stat2 <- rep(0, length(y))
  pval1 <- rep(0.5, length(y))
  pval2 <- rep(0.5, length(y))

  z <- z1
  test_stat <- test_stat1
  pval <- pval1

  z1[1] <- y[1]
  z2[1] <- y[1]

  for (i in 2:length(y)) {
    z1[i] <- w[1] * y[i] + (1 - w[1]) * z1[i - 1]
    z2[i] <- w[2] * y[i] + (1 - w[2]) * z2[i - 1]

    if (i >= B + g + 1) {
      test_stat1[i] <- (z1[i] - mu[i]) / sigma1[i]
      test_stat2[i] <- (z2[i] - mu[i]) / sigma2[i]

      if (abs(test_stat1[i]) > alert_thresh) {
        z1[i] <- mu[i] + sign(test_stat1[i]) * alert_thresh * sigma1[i]
      }

      if (abs(test_stat2[i]) > alert_thresh) {
        z2[i] <- mu[i] + sign(test_stat2[i]) * alert_thresh * sigma2[i]
      }

      pval1[i] <- 1 - pt(test_stat1[i], df = B - 1)
      pval2[i] <- 1 - pt(test_stat2[i], df = B - 1)

      if (pval1[i] < pval2[i]) {
        pval[i] <- pval1[i]
        test_stat[i] <- test_stat1[i]
        z[i] <- z1[i]
      } else {
        pval[i] <- pval2[i]
        test_stat[i] <- test_stat2[i]
        z[i] <- z2[i]
      }
    }
  }

  list(p.value = pval, test_statistic = test_stat, ewma = z)
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
#' chosen geographic region.
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
#' df_ewma <- alert_ewma(df)
#'
#' head(df)
#' head(df_ewma)
#'
#' # Example 2
#' df <- data.frame(
#'   Date = seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 1),
#'   percent = runif(366)
#' )
#' df_ewma <- alert_ewma(df, t = Date, y = percent)
#'
#' head(df)
#' head(df_ewma)
#' \dontrun{
#' # Example 3: Data from NSSP-ESSENCE
#' library(ggplot2)
#'
#' myProfile <- Credentials$new(askme("Enter your username:"), askme())
#'
#' url <- "https://essence2.syndromicsurveillance.org/nssp_essence/api/timeSeries?endDate=20Nov20
#' &percentParam=ccddCategory&datasource=va_hosp&startDate=22Aug20&medicalGroupingSystem=essencesyndromes
#' &userId=2362&aqtTarget=TimeSeries&ccddCategory=cli%20cc%20with%20cli%20dd%20and%20coronavirus%20dd%20v2
#' &geographySystem=hospitalstate&detector=probregv2&timeResolution=daily&hasBeenE=1&stratVal=
#' &multiStratVal=geography&graphOnly=true&numSeries=0&graphOptions=multipleSmall&seriesPerYear=false
#' &nonZeroComposite=false&removeZeroSeries=true&sigDigits=true&startMonth=January&stratVal=&multiStratVal=geography
#' &graphOnly=true&numSeries=0&graphOptions=multipleSmall&seriesPerYear=false&startMonth=January&nonZeroComposite=false"
#'
#' url <- url %>% gsub("\n", "", .)
#'
#' api_data <- get_api_data(url)
#'
#' df <- api_data$timeSeriesData
#'
#' df_ewma <- alert_ewma(df, t = date, y = count)
#'
#' # Visualize alert for South Dakota
#' df_ewma_state <- df_ewma %>%
#'   filter(hospitalstate_display == "South Dakota")
#'
#' df_ewma_state %>%
#'   ggplot(aes(x = date, y = count)) +
#'   geom_line(color = "blue") +
#'   geom_point(data = subset(df_ewma_state, alert == "red"), color = "red") +
#'   geom_point(data = subset(df_ewma_state, alert == "yellow"), color = "yellow") +
#'   theme_bw() +
#'   labs(
#'     x = "Date",
#'     y = "Percent"
#'   )
#' }
#'
alert_ewma <- function(df, t = date, y = count, B = 28, g = 2, w1 = 0.4, w2 = 0.9) {
  alert_thresh <- qt(1 - 0.01, df = B - 1)
  warning_thresh <- qt(1 - 0.05, df = B - 1)

  w <- c(w1, w2)
  sigma_correction <- sqrt((w / (2 - w)) + (1 / B) - 2 * (1 - w)^(g + 1) * ((1 - (1 - w)^B) / B))

  constant <- (0.1289 - (0.2414 - 0.1826 * (1 - w)^4) * log(10 * 0.05)) * (w / warning_thresh)

  sigma_min <- (w / warning_thresh) * (1 + 0.5 * (1 - w)^2)

  grouping <- group_vars(df)

  t <- substitute(t)
  y <- substitute(y)

  df %>%
    as.data.table() %>%
    .[, t := as.Date(eval(t))] %>%
    .[, y := as.numeric(eval(y))] %>%
    .[, mu := lag(frollmean(y, n = B, fill = NA, align = "right"), g + 1), by = grouping] %>%
    .[, sigma := lag(frollapply(y, n = B, FUN = sd, fill = NA, align = "right"), g + 1), by = grouping] %>%
    .[, sigma1 := sigma * sigma_correction[1] + constant[1]] %>%
    .[, sigma2 := sigma * sigma_correction[2] + constant[2]] %>%
    .[, sigma1 := max(sigma1, sigma_min[1]), by = c(grouping, "t")] %>%
    .[, sigma2 := max(sigma2, sigma_min[2]), by = c(grouping, "t")] %>%
    .[, c("p.value", "test_statistic", "ewma") := ewma_loop(y, mu, sigma1, sigma2, B, g, w, alert_thresh), by = grouping] %>%
    .[, p.value := round(p.value, 7)] %>%
    .[!is.na(mu)] %>%
    .[, alert := fcase(
      p.value < 0.01, "red",
      p.value >= 0.01 & p.value < 0.05, "yellow",
      default = "blue"
    )] %>%
    .[, !c("t", "y", "mu", "sigma", "sigma1", "sigma2")] %>%
    as.data.frame()
}
