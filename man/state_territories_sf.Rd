% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/state_territories_sf.R
\docType{data}
\name{state_territories_sf}
\alias{state_territories_sf}
\title{US States and Territories Shapefile}
\format{
An object of class \code{sf} (inherits from \code{tbl_df}, \code{tbl}, \code{data.frame}) with 56 rows and 4 columns.
}
\usage{
state_territories_sf
}
\description{
Useful \code{sf} object shapefile for US States and Territories.
}
\details{
\preformatted{
Rows: 56
Columns: 4
$ STATEFP  <chr> "01", "02", "04", "05", "06", "08", "09", "10", "11", "12", "13", "15", "16"…
$ STUSPS   <chr> "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID"…
$ NAME     <chr> "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Conne…
$ geometry <MULTIPOLYGON [°]> MULTIPOLYGON (((-85.00065 3..., MULTIPOLYGON (((-113.0281 2...,…
}
}
\examples{
library(Rnssp)
data("state_territories_sf")

plot(state_territories_sf$geometry)
}
\keyword{datasets}
