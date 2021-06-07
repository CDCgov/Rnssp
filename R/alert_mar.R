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
#' @return A data frame
#' @export
#'
#' @examples
#' # Example 1
#' df <- data.frame(date = seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 1),
#'                  count = floor(runif(366, min=0, max=101)))
#' df_mar <- alert_mar(df)
#'
#' head(df)
#' head(df_mar)
#'
#' # Example 2
#' df <- data.frame(Date = seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 1),
#'                  percent = runif(366))
#' df_mar <- alert_mar(df, t = Date, y = percent)
#'
#' head(df)
#' head(df_mar)
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
#' df_mar <- alert_mar(df, t = date, y = count)
#'
#' # Visualize alert for South Dakota
#' df_mar_state <- df_mar %>%
#'   filter(hospitalstate_display == "South Dakota")
#'
#' df_mar_state %>%
#'   ggplot(aes(x = t, y = count)) +
#'   geom_line(color = "blue") +
#'   geom_point(data = subset(df_mar_state, alert == "red"), color = "red") +
#'   geom_point(data = subset(df_mar_state, alert == "yellow"), color = "yellow") +
#'   theme_bw() +
#'   labs(x = "Date",
#'        y = "Percent")
#' }
#'

alert_mar <- function(df, t = date, y = count, B = 28, g = 2){

  grouping <- group_vars(df)

  t <- enquo(t)
  y <- enquo(y)

  df %>%
    mutate(
      t = as.Date(!!t),
      y = as.numeric(!!y),
      row = row_number(),
      days = weekdays(t, abbreviate = TRUE),
      initial_vals = ifelse(row <= B, "yes", "no"),
      var = 1
    ) %>%
    tidyr::pivot_wider(names_from = days, values_from = var, values_fill = 0) %>%
    mutate(
      detection = slider::slide(
        .x = tibble(t, y, Mon, Tue, Wed, Thu, Fri, Sat),
        .f = function(.x){

          .current <- .x %>%
            mutate(X1 = seq(1, B + g + 1, 1)) %>%
            slice_tail(n = 1)

          .baseline <- head(.x, n = B)
          .y <- last(.x$y)

          .model <- .baseline %>%
            mutate(X1 = seq(1, B, 1)) %>%
            lm(y ~ X1 + Mon + Tue + Wed + Thu + Fri + Sat, .)

          .pred <- predict(.model, newdata = .current, se.fit = TRUE)
          .fitted <- .pred$fit[1]
          .mse <- .pred$residual.scale[1]^2
          .sigma <- sqrt(.pred$se.fit[1]^2 + .mse)

          .statistic <- (.y - .fitted) / .sigma
          .p.value <- pt(-abs(.statistic), df = B - 7 - 1)

          data.frame(fitted = .fitted, statistic = .statistic, p.value = .p.value) %>%
            mutate(
              alert = case_when(
                statistic > 0 & p.value < 0.01 ~ "red",
                statistic > 0 & 0.01 <= p.value & p.value < 0.05 ~ "yellow",
                TRUE ~ "none"
              )
            )
        },
        .before = B + g,
        .complete = TRUE
      )
    ) %>%
    tidyr::unnest(detection)

}
