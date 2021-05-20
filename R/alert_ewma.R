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
#' @param df A data frame, data frame extension (e.g. a tibble), or a lazy data frame.
#' @param t Name of the column of type Date containing the dates
#' @param y Name of the column of type Numeric containing counts or percentages
#' @param B Baseline parameter. The baseline length is the number of days from which to
#'     compute the EWMA and compare to in order to form the test statistic (default is 28)
#' @param g Guardband paramter. The guardband length is the number of days separating
#'     the baseline from the current date in consideration for alerting (default is 2)
#' @param w Smoothing parameter. The smoothing parameter controls the amount of
#'     smoothing in the exponentially weighted moving average (default is 0.4)
#'
#' @return A data frame
#' @export
#'
#' @examples
#' # Example 1
#' df <- data.frame(date = seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 1),
#'                  count = floor(runif(366, min=0, max=101)))
#' df_ewma <- alert_ewma(df)
#'
#' head(df)
#' head(df_ewma)
#'
#' # Example 2
#' df <- data.frame(Date = seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 1),
#'                  percent = runif(366))
#' df_ewma <- alert_ewma(df, t = Date, y = percent)
#'
#' head(df)
#' head(df_ewma)
#'
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
#' api_data <- myProfile$get_api_data(url)
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
#'   ggplot(aes(x = t, y = count)) +
#'   geom_line(color = "blue") +
#'   geom_point(data = subset(df_ewma_state, alert == "red"), color = "red") +
#'   geom_point(data = subset(df_ewma_state, alert == "yellow"), color = "yellow") +
#'   theme_bw() +
#'   labs(x = "Date",
#'        y = "Percent")
#' }
#'

alert_ewma <- function(df, t = date, y = count, B = 28, g = 2, w = 0.4){

  sigma_min <- 0.5
  sigma_correction <- sqrt((w/(2 - w)) + (1/B) - 2*(1 - w)^(g + 1)*((((1 - w)^B) - 1)/B))

  grouping <- group_vars(df)

  t <- substitute(t)
  y <- substitute(y)

  df %>%
    as.data.table() %>%
    .[, t := as.Date(eval(t))] %>%
    .[, y := as.numeric(eval(y))] %>%
    .[, mu := lag(frollmean(y, n = B, align = "right"), g + 1), by = grouping] %>%
    .[, s := lag(frollapply(y, n = B, FUN = sd, align = "right"), g + 1), by = grouping] %>%
    .[, s := max(0.5, s), by = c(grouping, "t")] %>%
    .[, z := accumulate(.x = y, ~w * .y + (1 - w) * .x, init = first(y)), by = grouping] %>%
    .[, test_statistic := (z - mu) / (s * sigma_correction)] %>%
    .[, p.value := pt(-abs(test_statistic), df = B - 1)] %>%
    .[, alert := fcase(
      p.value < 0.01 & test_statistic > 0, "red",
      p.value >= 0.01 & p.value < 0.05 & test_statistic > 0, "yellow",
      default = "blue"
    )] %>%
    .[!is.na(mu), ]
}
