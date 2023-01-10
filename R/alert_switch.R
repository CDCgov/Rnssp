#' Regression/EWMA Switch
#'
#' The NSSP-ESSENCE Regression/EWMA Switch algorithm generalized the Regression and
#' EWMA algorithms by applying the most appropriate algorithm for the data in the
#' baseline. First, multiple adaptive regression is applied where the adjusted R
#' squared value of the model is examined to see if it meets a threshold of 0.60. If
#' this threshold is not met, then the model is considered to not explain the data well.
#' In this case, the algorithm switches to the EWMA algorithm, which is more appropriate
#' for sparser time series that are common with county level trends. The smoothing
#' coefficient for the EWMA algorithm is fixed to 0.4.
#'
#' @param df A data frame, data frame extension (e.g. a tibble), or a lazy data frame.
#' @param t Name of the column of type Date containing the dates
#' @param y Name of the column of type Numeric containing counts or percentages
#' @param B Baseline parameter. The baseline length is the number of days to which
#' each liner model is fit (default is 28)
#' @param g Guardband paramter. The guardband length is the number of days separating
#'     the baseline from the current date in consideration for alerting (default is 2)
#' @param w1 Smoothing coefficient for sensitivity to gradual events. Must be between
#'     0 and 1 and is recommended to be between 0.3 and 0.5 to account for gradual
#'     effects. Defaults to 0.4 to match ESSENCE implementation.
#' @param w2 Smoothed coefficient for sensitivity to sudden events. Must be between
#'     0 and 1 and is recommended to be above 0.7 to account for sudden events.
#'     Defaults to 0.9 to match ESSENCE implementation and approximate the C2 algorithm.
#' @return A data frame
#' @export
#'
#' @examples
#' # Example 1
#' df <- data.frame(
#'   date = seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 1),
#'   count = floor(runif(366, min = 0, max = 101))
#' )
#' df_switch <- alert_switch(df)
#'
#' head(df)
#' head(df_switch)
#'
#' # Example 2
#' df <- data.frame(
#'   Date = seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 1),
#'   percent = runif(366)
#' )
#' df_switch <- alert_switch(df, t = Date, y = percent)
#'
#' head(df)
#' head(df_switch)
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
#' df_switch <- df %>%
#'   group_by(hospitaldhhsregion_display) %>%
#'   alert_switch(t = date, y = dataCount)
#'
#' # Visualize alert for HHS Region 4
#' df_switch_region <- df_switch %>%
#'   filter(hospitaldhhsregion_display == "Region 4")
#'
#' df_switch_region %>%
#'   ggplot() +
#'   geom_line(aes(x = date, y = dataCount), color = "grey70") +
#'   geom_line(
#'     data = subset(df_switch_region, alert != "grey"),
#'     aes(x = date, y = dataCount), color = "navy"
#'   ) +
#'   geom_point(
#'     data = subset(df_switch_region, alert == "blue"),
#'     aes(x = date, y = dataCount), color = "navy"
#'   ) +
#'   geom_point(
#'     data = subset(df_switch_region, alert == "yellow"),
#'     aes(x = date, y = dataCount), color = "yellow"
#'   ) +
#'   geom_point(
#'     data = subset(df_switch_region, alert == "red"),
#'     aes(x = date, y = dataCount), color = "red"
#'   ) +
#'   theme_bw() +
#'   labs(
#'     x = "Date",
#'     y = "Count"
#'   )
#' }
#'
alert_switch <- function(df, t = date, y = count, B = 28, g = 2, w1 = 0.4, w2 = 0.9) {

  # Check baseline length argument
  if (B < 7) {
    cli::cli_abort("Error in {.fn alert_switch}: baseline length argument {.var B} must be greater than or equal to 7")
  }

  if (B %% 7 != 0) {
    cli::cli_abort("Error in {.fn alert_switch}: baseline length argument {.var B} must be a multiple of 7")
  }

  # Check guardband length argument
  if (g < 0) {
    cli::cli_abort("Error in {.fn alert_switch}: guardband length argument {.var g} cannot be negative")
  }

  # Check for sufficient baseline data
  if (nrow(df) < B + g + 1) {
    cli::cli_abort("Error in {.fn alert_switch}: not enough historical data")
  }

  # Check for grouping variables
  grouped_df <- is.grouped_df(df)

  t <- enquo(t)
  y <- enquo(y)

  base_tbl <- df %>%
    mutate({{ t }} := as.Date(!!t))

  alert_tbl_reg <- base_tbl %>%
    alert_regression(t = !!t, y = !!y, B = B, g = g) %>%
    select(-sigma) %>%
    mutate(detector = "Adaptive Multiple Regression")

  alert_tbl_ewma <- base_tbl %>%
    alert_ewma(t = !!t, y = !!y, B = B, g = g, w1 = w1, w2 = w2) %>%
    mutate(detector = "EWMA")

  join_cols <- setdiff(names(alert_tbl_reg), c("baseline_expected", "test_statistic", "p.value", "adjusted_r_squared", "alert", "detector"))

  replace_dates <- alert_tbl_reg %>%
    filter(is.na(adjusted_r_squared) | adjusted_r_squared < 0.60) %>%
    select(-c(baseline_expected, test_statistic, p.value, adjusted_r_squared, alert, detector)) %>%
    inner_join(alert_tbl_ewma, by = join_cols)

  if (grouped_df) {
    groups <- group_vars(base_tbl)

    combined_out <- alert_tbl_reg %>%
      filter(adjusted_r_squared >= 0.60) %>%
      select(-adjusted_r_squared) %>%
      bind_rows(replace_dates) %>%
      arrange(!!sym(groups), !!enquo(t)) %>%
      mutate(detector = ifelse(is.na(test_statistic), NA, detector))
  } else {
    combined_out <- alert_tbl_reg %>%
      filter(adjusted_r_squared >= 0.60) %>%
      select(-adjusted_r_squared) %>%
      bind_rows(replace_dates) %>%
      arrange(!!enquo(t)) %>%
      mutate(detector = ifelse(is.na(test_statistic), NA, detector))
  }

  return(combined_out)
}
