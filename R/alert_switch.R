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
#'
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
#' df_switch <- alert_switch(df, t = date, y = count)
#'
#' # Visualize alert for South Dakota
#' df_switch_state <- df_switch %>%
#'   filter(hospitalstate_display == "South Dakota")
#'
#' df_switch_state %>%
#'   ggplot(aes(x = date, y = count)) +
#'   geom_line(color = "blue") +
#'   geom_point(data = subset(df_switch_state, alert == "alert"), color = "red") +
#'   geom_point(data = subset(df_switch_state, alert == "warning"), color = "yellow") +
#'   theme_bw() +
#'   labs(
#'     x = "Date",
#'     y = "Percent"
#'   )
#' }
#'
alert_switch <- function(df, t = date, y = count, B = 28, g = 2, w = 0.4) {

  grouping <- group_vars(df)

  col_names <- setdiff(names(df), c(ensym(t), ensym(y), grouping))

  tph <- quo_name(enquo(t))
  yph <- quo_name(enquo(y))

  t <- substitute(t)
  y <- substitute(y)

  reg_out <- df %>%
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
    .[, detection_out := lapply(baseline, detection_reg, algorithm = "switch", B, g)] %>%
    .[, !"baseline"] %>%
    .[, c("expected_switch", "test_statistic", "r_sqrd_adj") := tstrsplit(detection_out, "|", fixed = TRUE)] %>%
    .[, !"detection_out"] %>%
    .[, c("expected_switch", "test_statistic", "r_sqrd_adj") := lapply(.SD, gsub, pattern = "[a-zA-Z]*\\: ", replacement = "", perl = TRUE), .SDcols = c("expected_switch", "test_statistic", "r_sqrd_adj")] %>%
    .[, c("expected_switch", "test_statistic", "r_sqrd_adj") := lapply(.SD, as.numeric), .SDcols = c("expected_switch", "test_statistic", "r_sqrd_adj")] %>%
    .[, p.value := pt(-abs(test_statistic), df = B - 7 - 1)] %>%
    .[, p.value := fifelse(is.infinite(test_statistic) | is.nan(test_statistic), 0.5, p.value)] %>%
    .[, alert := fcase(test_statistic > 0 & p.value < 0.01, "alert",
                       test_statistic > 0 & p.value >= 0.01 & p.value < 0.05, "warning",
                       p.value == 0.5 & (is.infinite(test_statistic) | is.nan(test_statistic)) , "none",
                       default = "none")] %>%
    as.data.frame() %>%
    mutate(detector = "Adaptive Multiple Regression") %>%
    rename(
      !! tph := t,
      !! yph := y
    )

  sigma_min <- 0.5
  sigma_correction <- sqrt((w/(2 - w)) + (1/B) - 2*(1 - w)^(g + 1)*((((1 - w)^B) - 1)/B))

  ewma_out <- df %>%
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
      p.value < 0.01 & test_statistic > 0, "alert",
      p.value >= 0.01 & p.value < 0.05 & test_statistic > 0, "warning",
      default = "none"
    )] %>%
    .[!is.na(mu), ] %>%
    as.data.frame() %>%
    mutate(!! tph := t) %>%
    select(-c(t, y, s, z)) %>%
    rename(expected_switch = mu) %>%
    mutate(detector = "EWMA")

  join_cols <- setdiff(names(ewma_out), c("expected_switch", "test_statistic", "r_sqrd_adj", "p.value", "alert", "detector"))

  replace_dates <- reg_out %>%
    filter(r_sqrd_adj < 0.60) %>%
    select(-c(expected_switch, test_statistic, r_sqrd_adj, p.value, alert, detector)) %>%
    inner_join(ewma_out, by = join_cols)

  if (rlang::is_empty(grouping)) {
    combined_out <- reg_out %>%
      filter(r_sqrd_adj >= 0.60) %>%
      select(-r_sqrd_adj) %>%
      bind_rows(replace_dates) %>%
      arrange(!!enquo(t)))
  } else {
    combined_out <- reg_out %>%
      filter(r_sqrd_adj >= 0.60) %>%
      select(-r_sqrd_adj) %>%
      bind_rows(replace_dates) %>%
      arrange(!!ensym(grouping), !!enquo(t))
  }

  combined_out
}
