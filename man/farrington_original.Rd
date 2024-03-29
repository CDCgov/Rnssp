% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/alert_farrington.R
\name{farrington_original}
\alias{farrington_original}
\title{Original Farrington Algorithm}
\usage{
farrington_original(df, t = date, y = count, B = 4, w = 3)
}
\arguments{
\item{df}{Nested dataframe}

\item{t}{A column containing date values}

\item{y}{A column containing time series counts}

\item{B}{Number of years to include in baseline (default is 4)}

\item{w}{Half the number of weeks included in reference window,
before and after each reference date (default is 3)}
}
\value{
A tibble
}
\description{
Original Farrington Algorithm
}
\keyword{internal}
