#' Multiple Adaptive Regression
#'
#' The multiple adaptive regression algorithm fits a linear model to a baseline
#' of counts or percentages of length B, and forecasts a predicted value g + 1
#' days later (guard-band). This value is compared to the current observed value and
#' divided by the standard error of prediction in the test-statistic. The model includes
#' terms to account for linear trends and day-of-week effects. Note that this implementation
#' does NOT include holiday terms as in the Regression 1.2 algorithm in ESSENCE.
#' An alert is signaled if the statistical test (student's t-test) applied to the
#' test statistic yields a p-value less than 0.01. If the p-value is greater than or equal
#' to 0.01 and strictly less than 0.05, a warning is signaled.
#'
#' @param df A data frame, data frame extension (e.g. a tibble), or a lazy data frame.
#' @param t Name of the column of type Date containing the dates
#' @param y Name of the column of type Numeric containing counts or percentages
#' @param B Baseline parameter. The baseline length is the number of days to which
#' each liner model is fit (default is 28)
#' @param g Guardband parameter. The guardband length is the number of days separating
#'     the baseline from the current date in consideration for alerting (default is 2)
#'
#' @return A data frame with test statistic, p.value, and alert indicator
#' @export
#'
#' @examples
#' # Example 1
#' df <- data.frame(
#'   date = seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 1),
#'   count = floor(runif(366, min = 0, max = 101))
#' )
#' df_mar <- alert_mar(df)
#'
#' head(df)
#' head(df_mar)
#'
#' # Example 2
#' df <- data.frame(
#'   Date = seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 1),
#'   percent = runif(366)
#' )
#' df_mar <- alert_mar(df, t = Date, y = percent)
#'
#' head(df)
#' head(df_mar)
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
#' df_mar <- alert_mar(df, t = date, y = count)
#'
#' # Visualize alert for South Dakota
#' df_mar_state <- df_mar %>%
#'   filter(hospitalstate_display == "South Dakota")
#'
#' df_mar_state %>%
#'   ggplot(aes(x = date, y = count)) +
#'   geom_line(color = "blue") +
#'   geom_point(data = subset(df_mar_state, alert == "alert"), color = "red") +
#'   geom_point(data = subset(df_mar_state, alert == "warning"), color = "yellow") +
#'   theme_bw() +
#'   labs(
#'     x = "Date",
#'     y = "Percent"
#'   )
#' }
#'
alert_mar <- function(df, t = date, y = count, B = 28, g = 2) {
  grouping <- group_vars(df)

  col_names <- setdiff(names(df), c(ensym(t), ensym(y), grouping))

  tph <- quo_name(enquo(t))
  yph <- quo_name(enquo(y))

  t <- substitute(t)
  y <- substitute(y)

  df %>%
    as.data.table() %>%
    .[, t := as.Date(eval(t))] %>%
    .[, y := as.numeric(eval(y))] %>%
    .[, paste0("lag", 0:(B + g)) := shift(y, n = 0:(B + g)), by = grouping] %>%
    na.omit() %>%
    melt(measure.vars = paste0("lag", 0:(B + g)), variable.name = "var", value.name = "observed") %>%
    setorderv(c(grouping, "t", "var")) %>%
    .[, !"var"] %>%
    .[, base_date := seq_len(.N), by = c(grouping, "t")] %>%
    .[, base_date := t - base_date + 1] %>%
    .[, dow := weekdays(base_date, abbreviate = TRUE)] %>%
    .[, var := 1] %>%
    dcast(... ~ dow, value.var = "var", fill = 0) %>%
    setorderv(c(grouping, "t", "base_date")) %>%
    .[, .(.(.SD)), by = c(grouping, "t", "y", col_names)] %>%
    setnames(old = "V1", new = "baseline") %>%
    .[, test_statistic := as.numeric(lapply(baseline, detection_reg, "regression", B, g))] %>%
    .[, !"baseline"] %>%
    .[, p.value := pt(-abs(test_statistic), df = B - 7 - 1)] %>%
    .[, alert := fcase(test_statistic > 0 & p.value < 0.01, "alert",
      test_statistic > 0 & p.value >= 0.01 & p.value < 0.05, "warning",
      default = "none"
    )] %>%
    as.data.frame() %>%
    dplyr::rename(
      !!tph := t,
      !!yph := y
    )
}
