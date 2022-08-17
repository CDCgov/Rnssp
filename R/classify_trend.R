#' Trend Classification for Proportions/Percentages
#'
#' The algorithm fits rolling binomial models to a daily time series
#' of percentages or proportions in order to classify the overall
#' trend during the baseline period as significantly increasing,
#' significantly decreasing, or stable.
#'
#' @details The test statistic and p-value are extracted from each individual
#' model and are used in the following classification scheme:
#' \itemize{
#'     \item p-value < 0.01 and sign(test_statistic) > 0 ~ "Significant Increase"
#'     \item p-value < 0.01 and sign(test_statistic) < 0 ~ "Significant Decrease"
#'     \item p-value >= 0.01 ~ "Stable"
#' }
#'
#' If there are fewer than 10 encounters/counts in the baseline period,
#' a model is not fit and a value of NA is returned for the test statistic
#' and p-value
#'
#' @param df A data frame, data frame extension (e.g. a tibble), or a lazy data frame.
#' @param t Name of the column of type Date containing the dates
#' @param data_count Name of the column with counts for positive encounters
#' @param all_count Name of the column with total counts of encounters
#' @param B Baseline parameter. The baseline length is the number of days to which
#' each binomial model is fit (default is 12)
#'
#' @return A data frame. The first B rows within each group will be missing.
#' @export
#'
#' @examples
#' # Example 1
#' df <- data.frame(
#'   date = seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 1),
#'   dataCount = floor(runif(366, min = 0, max = 101)),
#'   allCount = floor(runif(366, min = 101, max = 500))
#' )
#'
#' df_trend <- classify_trend(df)
#' head(df_trend)
#' \dontrun{
#' # Example 2 with Data from NSSP-ESSENCE
#' library(ggplot2)
#' library(ggthemes)
#'
#' myProfile <- Credentials$new(askme("Enter your username:"), askme())
#'
#' url <- "https://essence2.syndromicsurveillance.org/nssp_essence/api/timeSeries?
#' endDate=20Nov20&percentParam=ccddCategory&datasource=va_hosp&startDate=22Aug20
#' &medicalGroupingSystem=essencesyndromes&userId=2362&aqtTarget=TimeSeries
#' &ccddCategory=cli%20cc%20with%20cli%20dd%20and%20coronavirus%20dd%20v2
#' &geographySystem=hospitalstate&detector=probregv2&timeResolution=daily&hasBeenE=1
#' &stratVal=&multiStratVal=geography&graphOnly=true&numSeries=0&graphOptions=multipleSmall
#' &seriesPerYear=false&nonZeroComposite=false&removeZeroSeries=true&sigDigits=true
#' &startMonth=January&stratVal=&multiStratVal=geography&graphOnly=true&numSeries=0
#' &graphOptions=multipleSmall&seriesPerYear=false&startMonth=January&nonZeroComposite=false"
#'
#' url <- url %>% gsub("\n", "", .)
#'
#' api_data <- myProfile$get_api_data(url)
#'
#' df <- api_data$timeSeriesData
#'
#' data_trend <- classify_trend(df, data_count = dataCount, all_count = allCount)
#'
#' # Visualize Montana State trend
#' pal <- c("#FF0000", "#1D8AFF", "#FFF70E", "grey90")
#'
#' data_trend %>%
#'   mutate(percent = data_count / all_count * 100) %>%
#'   filter(title == "Montana") %>%
#'   ggplot(., aes(x = t, y = percent)) +
#'   geom_line(color = pal[2], alpha = 0.5) +
#'   geom_hline(yintercept = -0.4, size = 4.5, color = "white") +
#'   geom_segment(aes(x = t, xend = max(t), y = -0.4, yend = -0.4, color = trend_classification), size = 3) +
#'   scale_color_manual(values = pal, name = "Trend Classification") +
#'   theme_few() +
#'   labs(
#'     title = "Percent of Emergency Department Visits with Diagnosed COVID-19",
#'     subtitle = "November 1st, 2020 to February 27th, 2020",
#'     x = "Date",
#'     y = "Percent"
#'   )
#' }
#'
classify_trend <- function(df, t = date, data_count = dataCount, all_count = allCount, B = 12) {
  grouping <- group_vars(df)
  all_count <- enquo(all_count)
  t <- enquo(t)
  data_count <- enquo(data_count)
  df %>%
    mutate(
      t = as.Date(!!t),
      data_count = as.numeric(!!data_count),
      all_count = as.numeric(!!all_count)
    ) %>%
    mutate(
      trend_analysis = slider::slide(
        .x = tibble(t, data_count, all_count),
        .f = function(.x) {
          if (sum(.x$data_count) > 10) {
            glm(cbind(data_count, all_count - data_count) ~ t, family = "binomial", data = .x) %>%
              broom::tidy() %>%
              filter(term == "t") %>%
              select(statistic, p.value)
          } else {
            data.frame(statistic = NA, p.value = NA)
          }
        },
        .before = B - 1,
        .complete = TRUE
      )
    ) %>%
    tidyr::unnest(trend_analysis) %>%
    ungroup() %>%
    mutate(
      trend_classification = case_when(
        p.value < 0.01 & statistic > 0 ~ "Significant Increase",
        p.value < 0.01 & statistic < 0 ~ "Significant Decrease",
        is.na(p.value) ~ "Insufficient Data",
        TRUE ~ "Stable"
      ),
      trend_classification = factor(trend_classification, levels = c("Significant Increase", "Significant Decrease", "Stable", "Insufficient Data"))
    )
}
