#' Fit linear regression model with time and day-of-week terms
#'
#' @param x A datatable object.
#' @param algorithm Specifies detection algorithm. Defaults to "regression".
#' @param B Baseline parameter. The baseline length is the number of days to
#'     which each linear model is fit. Defaults to 28 days.
#' @param g Guardband parameter. Number of days in buffer period to separate
#'     the test date from the baseline.
#' @return
#'     - A numeric value of test statistic for one-sided Student's t-test when
#'       algorithm is "regression"
#'     - A character string specifying the fitted value, test statistic, and
#'       adjusted R-squared when algorithm is "switch".
#'

detection_reg <- function (x, algorithm = "regression", B, g) {

  .matrix <- as.matrix(cbind(time = 1:B, x[1:B, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat")]))

  .model <- lm.fit(x = cbind(1, .matrix), y = x[1:B, ]$observed)
  .resid <- .model$residuals
  .mse <- (1 / (B - 7 - 1)) * sum(.resid ^ 2)
  .sigma <- sqrt(.mse) * sqrt(((B + 7) * (B - 4)) / (B * (B - 7)))
  .beta <- .model$coefficients

  .dow <- as.numeric(format(as.Date(last(x$base_date)), "%u"))

  .fit <- if (.dow < 7) {
    .beta[[1]] + (B + 2 + 1) * .beta[[2]] + .beta[[.dow + 2]]
  } else {
    .beta[[1]] + (B + 2 + 1) * .beta[[2]]
  }

  .test_statistic <- (last(x$observed) - .fit) / .sigma

  if (algorithm == "switch") {

    .f <- .model$fitted.values
    .mss <- sum((.f - mean(.f)) ^ 2)
    .rss <- sum(.resid ^ 2)
    .r_sqrd <- (.mss / (.rss + .mss))
    .r_sqrd_adj <- 1 - (1 - .r_sqrd) * ((nrow(.matrix) - 1) / .model$df.residual)
    .r_sqrd_adj <- if(is.nan(.r_sqrd_adj)) {
      0
    } else {
      .r_sqrd_adj
    }

    return(paste0("Fitted: ", .fit, "|TestStatistic: ", .test_statistic, "|AdjustedRSqrd: ", .r_sqrd_adj))
  } else {

    return(.test_statistic)

  }

}
