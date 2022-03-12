#' Adaptive Multiple Regression (DEPRECATED!)
#'
#' The adaptive multiple regression algorithm fits a linear model to a baseline
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
#' df_mar <- alert_mar(df)
#'
#' head(df)
#' head(df_mar)
#'
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
#' &ccddCategory=cli%20cc%20with%20cli%20dd%20and%20coronavirus%20dd%20v2&percentParam=ccddCategory
#' &geographySystem=hospitaldhhsregion&datasource=va_hospdreg&detector=probrepswitch&startDate=22Aug20
#' &timeResolution=daily&hasBeenE=1&medicalGroupingSystem=essencesyndromes&userId=2362&aqtTarget=TimeSeries
#' &stratVal=&multiStratVal=geography&graphOnly=true&numSeries=0&graphOptions=multipleSmall&seriesPerYear=false
#' &nonZeroComposite=false&removeZeroSeries=true&startMonth=January&stratVal=&multiStratVal=geography&graphOnly=true
#' &numSeries=0&graphOptions=multipleSmall&seriesPerYear=false&startMonth=January&nonZeroComposite=false"
#'
#' url <- url %>% gsub("\n", "", .)
#'
#' api_data <- get_api_data(url)
#'
#' df <- api_data$timeSeriesData
#'
#' df_mar <- df %>%
#'   group_by(hospitaldhhsregion_display) %>%
#'   alert_mar(t = date, y = dataCount)
#'
#' # Visualize alert for HHS Region 4
#' df_mar_region <- df_mar %>%
#'   filter(hospitaldhhsregion_display == "Region 4")
#'
#' df_mar_region %>%
#'   ggplot() +
#'   geom_line(aes(x = date, y = dataCount), color = "grey70") +
#'   geom_line(
#'     data = subset(df_mar_region, alert != "grey"),
#'     aes(x = date, y = dataCount), color = "navy"
#'   ) +
#'   geom_point(
#'     data = subset(df_mar_region, alert == "blue"),
#'     aes(x = date, y = dataCount), color = "navy"
#'   ) +
#'   geom_point(
#'     data = subset(df_mar_region, alert == "yellow"),
#'     aes(x = date, y = dataCount), color = "yellow"
#'   ) +
#'   geom_point(
#'     data = subset(df_mar_region, alert == "red"),
#'     aes(x = date, y = dataCount), color = "red"
#'   ) +
#'   theme_bw() +
#'   labs(
#'     x = "Date",
#'     y = "Count"
#'   )
#' }
#'
alert_mar <- function(df, t = date, y = count, B = 28, g = 2) {
  if (!identical(Sys.getenv("TESTTHAT"), "true")) {
    .Deprecated("alert_regression")
  }

  # Check baseline length argument
  if (B < 7) {
    cli::cli_abort("Error in {.fn alert_regression}: baseline length argument {.var B} must be greater than or equal to 7")
  }

  if (B %% 7 != 0) {
    cli::cli_abort("Error in {.fn alert_regression}: baseline length argument {.var B} must be a multiple of 7")
  }

  # Check guardband length argument
  if (g < 0) {
    cli::cli_abort("Error in {.fn alert_regression}: guardband length argument {.var g} cannot be negative")
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
      mutate(anomalies = map(.x = data_split, .f = adaptive_regression, t = !!t, y = !!y, B = B, g = g)) %>%
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
  } else {
    unique_dates <- base_tbl %>%
      pull(!!t) %>%
      unique()

    if (length(unique_dates) != nrow(base_tbl)) {
      cli::cli_abort("Error in {.fn alert_regression}: Number of unique dates does not equal the number of rows. Should your dataframe be grouped?")
    }

    base_tbl %>%
      nest(data_split = everything()) %>%
      mutate(anomalies = map(.x = data_split, .f = adaptive_regression, t = !!t, y = !!y, B = B, g = g)) %>%
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
  }
}
