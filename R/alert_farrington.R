#' Original Farrington Algorithm
#'
#' @param df Nested dataframe
#' @param t A column containing date values
#' @param y A column containing time series counts
#' @param B Number of years to include in baseline (default is 4)
#' @param w Half the number of weeks included in reference window, before and after each reference date (default is 3)
#'
#' @return A tibble
#'
#' @keywords internal
#'
#'
farrington_original <- function (df, t = date, y = data_count, B = 4, w = 3) {

  t <- enquo(t)
  y <- enquo(y)

  N <- nrow(df)

  # Minimum number of observations needed
  min_obs <- 52 * B + w + 2

  # Initialize result vectors
  predicted <- rep(NA, N)
  time_coefficient <- rep(NA, N)
  include_time_term <- rep(NA, N)
  upper <- rep(NA, N)
  alert_score <- rep(NA, N)
  alert <- rep(NA, N)

  dates <- df %>%
    pull(!!t)

  y_obs <- df %>%
    pull(!!y)

  for (i in min_obs:N) {

    current_date <- dates[i]

    ref_dates <- seq(current_date, length = B + 1, by = "-1 year")[-1]

    wday_gaps <- as.numeric(format(ref_dates, "%w")) - as.numeric(format(current_date, "%w"))
    ref_dates_shifted <- ref_dates - wday_gaps
    floor_ceiling_dates <- if_else(ref_dates_shifted > ref_dates, ref_dates_shifted - 7, ref_dates_shifted + 7)
    center_dates <- sort(if_else(abs(ref_dates - floor_ceiling_dates) < abs(ref_dates - ref_dates_shifted), floor_ceiling_dates, ref_dates_shifted))

    base_start <- sort((center_dates - 7 * w))[1:B]

    idx_start <- which(dates %in% base_start)
    idx <- rep(idx_start, each = 7) + 0:6

    min_date <- min(dates[idx])
    base_dates <- as.numeric((dates[idx] - min(dates[idx])) / 7)
    base_counts <- y_obs[idx]

    mod <- suppressWarnings(
      glm(base_counts ~ 1 + base_dates, family = quasipoisson(link = "log"))
    )

    if (!mod$converged) {

      mod <- suppressWarnings(
        glm(base_counts ~ 1, family = quasipoisson(link = "log"))
      )

      include_time <- FALSE

    } else{

      include_time <- TRUE

    }

    if (!mod$converged) {

      next

    }

    mod_formula <- mod$formula

    if (include_time) {

      time_coeff <- mod$coefficients[["base_dates"]]
      time_p_val <- summary(mod)$coefficients[2, 4]

    } else {

      time_coeff <- NA
      time_p_val <- NA

    }

    y_observed <- as.numeric(mod$y)
    y_fit <- as.numeric(mod$fitted.values)
    phi <- max(summary(mod)$dispersion, 1)
    diag <- as.numeric(hatvalues(mod))

    ambscombe_resid <- ((3 / 2) * (y_observed ^ (2 / 3) * (y_fit ^ (-1 / 6)) - sqrt(y_fit))) / (sqrt(phi * (1 - diag)))
    scaled <- if_else(ambscombe_resid > 1, 1 / (ambscombe_resid ^ 2), 1)
    gamma <- length(ambscombe_resid) / sum(scaled)
    omega <- if_else(ambscombe_resid > 1, gamma / (ambscombe_resid ^ 2), gamma)

    mod_weighted <- suppressWarnings(
      glm(as.formula(mod_formula), family = quasipoisson(link = "log"), weights = omega)
    )

    phi_weighted <- max(summary(mod_weighted)$dispersion, 1)

    mod_weighted$phi <- phi_weighted
    mod_weighted$weights <- omega
    mod_weighted$week_time <- base_dates
    mod_weighted$data_count <- base_counts

    if (include_time) {

      time_coeff_weighted <- mod_weighted$coefficients[["base_dates"]]
      time_pval_weighted <- summary(mod_weighted)$coefficients[2, 4]

    }

    pred_week_time <- as.numeric((current_date - min_date)) / 7

    pred <- predict.glm(
      mod_weighted,
      newdata = data.frame(
        base_dates = pred_week_time,
        dispersion = phi_weighted
      ),
      se.fit = TRUE,
      type = "response"
    )

    time_significant <- (pred$fit <= max(base_counts, na.rm = TRUE) & time_pval_weighted < 0.05)

    # Check 2 conditions

    # > 1: p-value for time term significant at 0.05 level
    time_significant <- time_pval_weighted < 0.05

    # > 2: Prediction less than or equal to maximum observation in baseline
    pred_ok <- pred$fit <= max(base_counts, na.rm = TRUE)

    trend <- include_time & time_significant & pred_ok

    if (!trend) {

      mod <- suppressWarnings(
        glm(base_counts ~ 1, family = quasipoisson(link = "log"))
      )

      if (!mod$converged) {

        next

      } else {

        mod_formula <- mod$formula

        y_observed <- as.numeric(mod$y)
        y_fit <- as.numeric(mod$fitted.values)
        phi <- max(summary(mod)$dispersion, 1)
        diag <- as.numeric(hatvalues(mod))

        ambscombe_resid <- ((3 / 2) * (y_observed ^ (2 / 3) * (y_fit ^ (-1 / 6)) - sqrt(y_fit))) / (sqrt(phi * (1 - diag)))
        scaled <- if_else(ambscombe_resid > 1, 1 / (ambscombe_resid ^ 2), 1)
        gamma <- length(ambscombe_resid) / sum(scaled)
        omega <- if_else(ambscombe_resid > 1, gamma / (ambscombe_resid ^ 2), gamma)

        mod_weighted <- suppressWarnings(
          glm(as.formula(mod_formula), family = quasipoisson(link = "log"), weights = omega)
        )

        phi_weighted <- max(summary(mod_weighted)$dispersion, 1)

        mod_weighted$phi <- phi_weighted
        mod_weighted$weights <- omega
        mod_weighted$week_time <- base_dates
        mod_weighted$data_count <- base_counts

        pred <- predict.glm(
          mod_weighted,
          newdata = data.frame(
            base_dates = pred_week_time,
            population = 1,
            dispersion = phi_weighted
          ),
          se.fit = TRUE,
          type = "response"
        )

        include_time_term[i] <- FALSE

      }

    }


    predicted[i] <- pred$fit
    time_coefficient[i] <- time_coeff
    include_time_term[i] <- TRUE

    # Temporary result vectors
    se_fit <- pred$se.fit
    tau <- phi_weighted + ((se_fit ^ 2) / predicted[i])
    se_pred <- sqrt((4 / 9) * predicted[i] ^ (1 / 3) * tau)
    # alert_p.value[i] <- pnorm(y_obs[i] ^ (2 / 3), mean = predicted[i] ^ (2 / 3), sd = se_pred, lower.tail = FALSE)

    upper[i] <- max(0, (predicted[i] ^ (2 / 3) + qnorm(0.95) * se_pred) ^ (3 / 2), na.rm = TRUE)

    alert_score[i] <- if_else(!is.na(upper[i]), (y_obs[i] - predicted[i]) / (upper[i] - predicted[i]), NA_real_)

    recent_counts <- sum(y_obs[(i - 4):i])
    alert[i] <- if_else(alert_score[i] > 1 & recent_counts > 5, TRUE, FALSE)

  }

  tibble(
    predicted = predicted,
    time_coefficient = time_coefficient,
    include_time_term = include_time_term,
    upper = upper,
    alert_score = alert_score,
    alert = alert
  )

}

#' Return 10-level seasonal factor vector
#'
#' @param B Number of years to include in baseline (default is 4)
#' @param g Number of guardband weeks to separate the test week from the baseline (default is 27)
#' @param w Half the number of weeks included in reference window, before and after each reference date (default is 3)
#' @param p Number of seasonal periods for each year in baseline (default is 10)
#' @param base_length Total number of weeks included in baseline
#' @param base_weeks Indices of baseline weeks
#'
#' @return A factor vector
#'
#' @keywords internal
#'
seasonal_groups <- function (B = 4, g = 27, w = 3, p = 10, base_length, base_weeks) {

  h <- c(1, diff(base_weeks))
  csum_h <- cumsum(h)

  fct_levels <- rep(0, base_length)

  for (i in 1:B) {

    fct_levels[csum_h[i]:(csum_h[i] + 2 * w)] <- p

    delta_weeks <- h[i + 1] - (2 * w + 1)

    quotient <- delta_weeks %/% (p - 1)
    remainder <- delta_weeks %% (p - 1)

    idx_extra <- seq_len(remainder)

    fct_lengths <- rep(quotient, p - 1)
    fct_lengths[idx_extra] <- fct_lengths[idx_extra] + 1
    fct_lengths <- c(0, fct_lengths)

    cum_lengths <- cumsum(fct_lengths)

    for (j in 1:(p - 1)) {

      fct_levels[(csum_h[i] + 2 * w + 1 + cum_lengths[j]):(csum_h[i] + 2 * w + cum_lengths[j + 1])] <- j

    }

  }

  # Trim extra components outside of baseline
  fct_levels <- as.factor(fct_levels[1:(length(fct_levels) - (g - 1) + w)])

}

#' Modified Farrington Algorithm
#'
#' @param df Nested dataframe
#' @param t A column containing date values
#' @param y A column containing time series counts
#' @param B Number of years to include in baseline (default is 4)
#' @param g Number of guardband weeks to separate the test date from the baseline (default is 27)
#' @param w Half the number of weeks included in reference window, before and after each reference date (default is 3)
#' @param p Number of seasonal periods for each year in baseline
#'
#' @return A tibble
#'
#' @keywords internal
#'
farrington_modified <- function (df, t = date, y = data_count, B = 4, g = 27, w = 3, p = 10) {

  t <- enquo(t)
  y <- enquo(y)

  N <- nrow(df)

  # Minimum number of observations needed
  min_obs <- 52 * B + w + 2

  # Initialize result vectors
  predicted <- rep(NA, N)
  time_coefficient <- rep(NA, N)
  include_time_term <- rep(NA, N)
  upper <- rep(NA, N)
  alert_score <- rep(NA, N)
  alert = rep(NA, N)

  dates <- df %>%
    pull(!!t)

  y_obs <- df %>%
    pull(!!y)

  for (i in min_obs:N) {

    current_date <- dates[i]

    ref_dates <- seq(current_date, length = B + 1, by = "-1 year")

    wday_gaps <- as.numeric(format(ref_dates, "%w")) - as.numeric(format(current_date, "%w"))
    ref_dates_shifted <- ref_dates - wday_gaps
    floor_ceiling_dates <- if_else(ref_dates_shifted > ref_dates, ref_dates_shifted - 7, ref_dates_shifted + 7)
    center_dates <- sort(if_else(abs(ref_dates - floor_ceiling_dates) < abs(ref_dates - ref_dates_shifted), floor_ceiling_dates, ref_dates_shifted))

    base_start <- sort((center_dates - 7 * w))[1:B]
    base_end <- c(sort(center_dates - 7 * B)[2:B], max(center_dates) - 7 * g)

    base_dates <- seq(min(base_start), max(base_end), by = "1 week")
    base_length <- length(base_dates)
    base_weeks <- which(dates %in% center_dates)

    fct_levels <- seasonal_groups(B, g, w, p, base_length, base_weeks)

    idx <- which(dates %in% base_dates)

    min_date <- min(dates[idx])
    base_dates <- as.numeric((dates[idx] - min(dates[idx])) / 7)
    base_counts <- y_obs[idx]

    mod <- suppressWarnings(
      glm(base_counts ~ 1 + base_dates + fct_levels, family = quasipoisson(link = "log"))
    )

    if (!mod$converged) {

      mod <- suppressWarnings(
        glm(base_counts ~ 1 + fct_levels, family = quasipoisson(link = "log"))
      )

      include_time <- FALSE

    } else{

      include_time <- TRUE

    }

    if (!mod$converged) {

      next

    }

    mod_formula <- mod$formula

    if (include_time) {

      time_coeff <- mod$coefficients[["base_dates"]]
      time_p_val <- summary(mod)$coefficients[2, 4]

    } else {

      time_coeff <- NA
      time_p_val <- NA

    }

    y_observed <- as.numeric(mod$y)
    y_fit <- as.numeric(mod$fitted.values)
    phi <- max(summary(mod)$dispersion, 1)
    diag <- as.numeric(hatvalues(mod))

    ambscombe_resid <- ((3 / 2) * (y_observed ^ (2 / 3) * (y_fit ^ (-1 / 6)) - sqrt(y_fit))) / (sqrt(phi * (1 - diag)))
    scaled <- if_else(ambscombe_resid > 2.58, 1 / (ambscombe_resid ^ 2), 1)
    gamma <- length(ambscombe_resid) / sum(scaled)
    omega <- if_else(ambscombe_resid > 2.58, gamma / (ambscombe_resid ^ 2), gamma)

    mod_weighted <- suppressWarnings(
      glm(as.formula(mod_formula), family = quasipoisson(link = "log"), weights = omega)
    )

    phi_weighted <- max(summary(mod_weighted)$dispersion, 1)

    mod_weighted$phi <- phi_weighted
    mod_weighted$weights <- omega
    mod_weighted$week_time <- base_dates
    mod_weighted$data_count <- base_counts

    if (include_time) {

      time_coeff_weighted <- mod_weighted$coefficients[["base_dates"]]
      time_pval_weighted <- summary(mod_weighted)$coefficients[2, 4]

    }

    pred_week_time <- as.numeric((current_date - min_date)) / 7

    pred <- predict.glm(
      mod_weighted,
      newdata = data.frame(
        base_dates = pred_week_time,
        dispersion = phi_weighted,
        fct_levels = factor(p)
      ),
      se.fit = TRUE,
      type = "response"
    )

    time_significant <- (pred$fit <= max(base_counts, na.rm = TRUE) & time_pval_weighted < 0.05)

    # Check 2 conditions

    # > 1: p-value for time term significant at 0.05 level
    time_significant <- time_pval_weighted < 0.05

    # > 2: Prediction less than or equal to maximum observation in baseline
    pred_ok <- pred$fit <= max(base_counts, na.rm = TRUE)

    trend <- include_time & time_significant & pred_ok

    if (!trend) {

      mod <- suppressWarnings(
        glm(base_counts ~ 1 + fct_levels, family = quasipoisson(link = "log"))
      )

      if (!mod$converged) {

        next

      } else {

        mod_formula <- mod$formula

        y_observed <- as.numeric(mod$y)
        y_fit <- as.numeric(mod$fitted.values)
        phi <- max(summary(mod)$dispersion, 1)
        diag <- as.numeric(hatvalues(mod))

        ambscombe_resid <- ((3 / 2) * (y_observed ^ (2 / 3) * (y_fit ^ (-1 / 6)) - sqrt(y_fit))) / (sqrt(phi * (1 - diag)))
        scaled <- if_else(ambscombe_resid > 2.58, 1 / (ambscombe_resid ^ 2), 1)
        gamma <- length(ambscombe_resid) / sum(scaled)
        omega <- if_else(ambscombe_resid > 2.58, gamma / (ambscombe_resid ^ 2), gamma)

        mod_weighted <- suppressWarnings(
          glm(as.formula(mod_formula), family = quasipoisson(link = "log"), weights = omega)
        )

        phi_weighted <- max(summary(mod_weighted)$dispersion, 1)

        mod_weighted$phi <- phi_weighted
        mod_weighted$weights <- omega
        mod_weighted$week_time <- base_dates
        mod_weighted$data_count <- base_counts

        pred <- predict.glm(
          mod_weighted,
          newdata = data.frame(
            base_dates = pred_week_time,
            population = 1,
            dispersion = phi_weighted,
            fct_levels = factor(p)
          ),
          se.fit = TRUE,
          type = "link"
        )

        include_time_term[i] <- FALSE

      }

    } else {

      pred <- predict.glm(
        mod_weighted,
        newdata = data.frame(
          base_dates = pred_week_time,
          population = 1,
          dispersion = phi_weighted,
          fct_levels = factor(p)
        ),
        se.fit = TRUE,
        type = "link"
      )

      include_time_term[i] <- TRUE

    }

    predicted[i] <- pred$fit
    time_coefficient[i] <- time_coeff
    include_time_term[i] <- TRUE

    # Temporary result vectors
    eta <- predicted[i]
    mu_q <- exp(eta)
    dispersion <- phi_weighted

    upper[i] <- if (mu_q == Inf) {
      NA_real_
    } else if (phi_weighted > 1) {
      qnbinom(p = 0.95, mu_q / (phi_weighted - 1), 1 / phi_weighted)
    } else {
      qpois(0.95, mu_q)
    }

    alert_score[i] <- if_else(!is.na(upper[i]), (y_obs[i] - predicted[i]) / (upper[i] - predicted[i]), NA_real_)

    recent_counts <- sum(y_obs[(i - 4):i])
    alert[i] <- if_else(alert_score[i] > 1 & recent_counts > 5, TRUE, FALSE)
    upper[i] <- if_else(recent_counts > 5, upper[i], NA_real_)
    predicted[i] <- exp(predicted[i])

  }

  tibble(
    predicted = predicted,
    time_coefficient = time_coefficient,
    include_time_term = include_time_term,
    upper = upper,
    alert_score = alert_score,
    alert = alert
  )

}


#' Farrington Temporal Detector
#'
#' The Farrington algorithm is intended for weekly time series of counts
#' spanning multiple years.
#'
#' Original Farrington Algorithm: Quasi-Poisson generalized linear regression models
#' are fit to baseline counts associated with reference dates in the B previous
#' years, including w weeks before and after each reference date. The algorithm
#' checks for convergence with a time term and refits a model with only an
#' intercept term in the scenario the model does not converge. The inclusion of
#' high baseline counts associated with past outbreaks or public health events is
#' known to result in alerting thresholds that are too high and a reduction in
#' sensitivity. An empirically derived weighting function is used to calculate
#' weights from Anscombe residuals that assign low weight to baseline observations
#' with large residuals. A 2/3rds transformation is applied to account for
#' skewness common to time series with lower counts, after which expected value
#' and variance estimates are used to derive upper and lower bounds for the
#' prediction interval. The alert score is defined as the current observation minus
#' the forecast value divided by the upper prediction interval bound minus the
#' forecast value. If this score exceeds 1, an alert is raised given that the number
#' of counts in the last 4 days is above 5. This algorithm requires that the number
#' of years included in the baseline is 3 or higher.
#'
#' Modified Farrington Algorithm: In 2012, Angela Noufaily developed a modified
#' implementation of the original Farrington algorithm that improved performance by
#' including more historical data in the baseline. The modified algorithm includes
#' all weeks from the beginning of the first reference window to the last week proceeding
#' a 27-week guardband period used to separate the test week from the baseline. A 10-level
#' factor is used to account for seasonality throughout the baseline. Additionally, the
#' modified algorithm assumes a negative binomial distribution on the weekly time series
#' counts, where thresholds are computed as quantiles of the negative binomial distribution
#' with plug-in estimates for mu and phi.
#'
#' @param df A dataframe, dataframe extension (e.g., a \code{tibble}), or a lazy dataframe
#' @param t A column containing date values
#' @param y A column containing time series counts
#' @param B An integer specifying the number of baseline years (default is 4)
#' @param w An integer specifying the number of weeks to include before and after the
#'     reference date (default is 3)
#' @param method A string of either "\code{original}" (default) or "\code{modified}" to
#'     specify the version of the Farrington algorithm (original vs modified).
#'
#' @return A dataframe
#'
#' @references
#' \itemize{
#' \item \href{https://www.jstor.org/stable/2983331?seq=1#metadata_info_tab_contents}{A Statistical Algorithm for the Early Detection of Outbreaks of Infectious Disease}
#'
#' \item \href{https://cran.r-project.org/web/packages/surveillance/vignettes/monitoringCounts.pdf}{Monitoring Count Time Series in R: Aberration Detection in Public Health Surveillance}
#' }
#'
#' @export
#'
#' @examples
#' # Example 1
#'
#' df <- data.frame(
#'  date = seq(as.Date("2014-01-05"), as.Date("2022-02-05"), "weeks"),
#'  count = rpois(length(seq(as.Date("2014-01-05"), as.Date("2022-02-05"), "weeks")), 25)
#' )
#'
#' ## Original Farrington algorithm
#'
#' df_farr_original <- alert_farrington(df, t = date, y = count)
#'
#'
#' ## Modified Farrington algorithm
#'
#' df_farr_modified <- alert_farrington(df, t = date, y = count, method = "modified")
#'
#'
#' \dontrun{
#' # Example 2: Data from NSSP-ESSENCE, national counts for CDC Respiratory Synctial Virus v1
#'
#' library(Rnssp)
#'
#' myProfile <- Credentials$new(askme("Enter your username:"), askme())
#'
#' url <- "https://essence2.syndromicsurveillance.org/nssp_essence/api/timeSeries?endDate=12
#' Feb2022&ccddCategory=cdc%20respiratory%20syncytial%20virus%20v1&percentParam=noPercent
#' &geographySystem=hospitaldhhsregion&datasource=va_hospdreg&detector=nodetectordetector
#' &startDate=29Dec2013&timeResolution=weekly&hasBeenE=1&medicalGroupingSystem=essencesyndromes
#' &userId=2362&aqtTarget=TimeSeries"
#'
#'
#' url <- url %>% gsub("\n", "", .)
#'
#' api_data <- get_api_data(url)
#'
#' df <- api_data$timeSeriesData
#'
#'
#' ## Original Farrington algorithm
#'
#' df_farr_original <- alert_farrington(df, t = date, y = count)
#'
#'
#' ## Modified Farrington algorithm
#'
#' df_farr_modified <- alert_farrington(df, t = date, y = count, method = "modified")
#'
#'
#' ### Visualize alert
#' df_farr_modified %>%
#'   ggplot() +
#'   geom_line(aes(x = date, y = count), size = 0.4, color = "grey70") +
#'   geom_line(data = subset(df_modified, !is.na(alert)), aes(x = date, y = count), size = 0.4, color = "navy") +
#'   geom_point(data = subset(df_modified, !alert), aes(x = date, y = count), size = 1.5, color = "navy") +
#'   geom_point(data = subset(df_modified, alert), aes(x = date, y = count), color = "red", size = 1.5) +
#'   theme_bw() +
#'   labs(
#'     x = "Date",
#'     y = "Weekly ED Visits"
#'   )
#' }
#'

alert_farrington <- function(df, t = date, y = data_count, B = 4, w = 3, method = "original") {

  # Ensure that df is a dataframe
  if (!is.data.frame(df)) {
    cli::cli_abort("Argument {.var df} must be a dataframe, tibble, or data.table")
  }

  # Check baseline length argument
  if (B < 4) {
    cli::cli_abort("Baseline length argument {.var B} must be greater than or equal to 4. Farrington algorithm requires a baseline of four or more years.")
  }

  # Check half-week baseline argument
  if (w < 0) {
    cli::cli_abort("Half-week baseline argument {.var w} cannot be negative")
  }

  # Check for sufficient baseline data:
  if (nrow(df) < 52 * B + w + 2) {
    cli::cli_abort("Not enough historical data to form baseline")
  }

  date_check <- df %>%
    pull(!!enquo(t))

  # Check for data that should be grouped
  if (!is_grouped_df(df) & length(unique(date_check)) != length(date_check)) {
    cli::cli_abort("Duplicate dates detected. Please group your dataframe!")
  }

  # Ensure that dates are in standard format
  format_dates <- try(as.Date(date_check), silent = TRUE)

  if (class(format_dates) == "try-error") {
    cli::cli_abort("Date argument {.var t} is not in a standard unambiguous format. Dates must be in {.code %Y-%m-%d} format.")
  }

  # Check if time series data is on a time resolution other than weekly
  h_dates <- unique(diff(as.Date(unique(date_check))))
  if (h_dates != 7) {
    cli::cli_abort("Distance between dates is not 7 days. Counts must be weekly!")
  }

  # Check for grouping variables
  grouped_df <- is.grouped_df(df)

  t <- enquo(t)
  y <- enquo(y)

  if (grouped_df) {

    groups <- group_vars(df)

    if (method == "modified") {

      alert_tbl <- df %>%
        mutate({{ t }} := as.Date(!!t)) %>%
        nest(data_split = -all_of(groups)) %>%
        mutate(anomalies = map(.x = data_split, .f = farrington_modified, t = !!t, y = !!y, B = 4, g = 27, w = 3, p = 10)) %>%
        unnest(c(data_split, anomalies))

    } else if (method == "original") {

      alert_tbl <- df %>%
        mutate({{ t }} := as.Date(!!t)) %>%
        nest(data_split = -all_of(groups)) %>%
        mutate(anomalies = map(.x = data_split, .f = farrington_original, t = !!t, y = !!y, B = 4, w = 3)) %>%
        unnest(c(data_split, anomalies))

    } else {
      cli::cli_abort("Argument {.code method} must be {.code original} or {.code modified}.")
    }

  } else {

    if (method == "modified") {

      alert_tbl <- df %>%
        mutate({{ t }} := as.Date(!!t)) %>%
        nest(data_split = everything()) %>%
        mutate(anomalies = map(.x = data_split, .f = farrington_modified, t = !!t, y = !!y, B = 4, g = 27, w = 3, p = 10)) %>%
        unnest(c(data_split, anomalies))

    } else if (method == "original") {

      alert_tbl <- df %>%
        mutate({{ t }} := as.Date(!!t)) %>%
        nest(data_split = everything()) %>%
        mutate(anomalies = map(.x = data_split, .f = farrington_original, t = !!t, y = !!y, B = 4, w = 3)) %>%
        unnest(c(data_split, anomalies))

    } else{
      cli::cli_abort("Argument {.code method} must be {.code original} or {.code modified}.")
    }

  }

}
