% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hsa_sf.R
\docType{data}
\name{hsa_sf}
\alias{hsa_sf}
\title{US Hospital Service Area Shapefile}
\format{
An object of class \code{sf} (inherits from \code{data.frame}) with 3436 rows and 6 columns.
}
\usage{
hsa_sf
}
\description{
Useful \code{sf} object shapefile for US Hospital Service Area (HSA) Shapefile
}
\details{
\preformatted{
Rows: 3,436
Columns: 6
$ hsa_id     <chr> "1001", "1002", "1003", "1004", "1006", "1007", "1008", "1010", "1011", "1012", "1013", "1014", "1016", "~
$ state_abbr <chr> "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL~
$ hsa        <chr> "ALABASTER", "ALEXANDER CITY", "ANDALUSIA", "ANNISTON", "ASHLAND", "ATHENS", "ATMORE", "BESSEMER", "BIRMI~
$ name       <chr> " Alabaster", " Alexander City", " Andalusia", " Anniston", " Ashland", " Athens", " Atmore", " Bessemer"~
$ state_fips <chr> "01", "01", "01", "01", "01", "01", "01", "01", "01", "01", "01", "01", "01", "01", "01", "01", "01", "01~
$ geometry   <MULTIPOLYGON [m]> MULTIPOLYGON (((827131.3 -4..., MULTIPOLYGON (((916807.7 -4..., MULTIPOLYGON (((919051.4 -6.~
}
}
\examples{
library(Rnssp)
data("hsa_sf")

plot(hsa_sf$geometry)
}
\keyword{datasets}
