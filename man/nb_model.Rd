% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/alert_nbinom.R
\name{nb_model}
\alias{nb_model}
\title{Negative binomial regression model for weekly counts}
\usage{
nb_model(df, t, y, baseline_end, include_time)
}
\arguments{
\item{df}{A data frame, data frame extension (e.g. a tibble),
or a lazy data frame}

\item{t}{Name of the column of type Date containing the dates}

\item{y}{Name of the column of type Numeric containing counts}

\item{baseline_end}{Object of type Date defining the end of the
baseline/training period}

\item{include_time}{Logical indicating whether or not to include time term
in regression model}
}
\value{
A data frame.
}
\description{
Negative binomial model helper function for monitoring weekly
count time series with seasonality
}
\keyword{internal}
