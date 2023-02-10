#' US Health and Human Services Regions Shapefile
#'
#' Useful \code{sf} object shapefile for US Health and Human Services (HHS) Regions.
#'
#' \preformatted{
#' Rows: 10
#' Columns: 4
#' $ region   <fct> Region 1, Region 2, Region 3, Region 4, Region 5, Region 6, Region 7, Region 8, Region 9, Region 10
#' $ geometry <GEOMETRY [°]> MULTIPOLYGON (((-71.59999 4..., MULTIPOLYGON (((-74.21979 4..., MULTIPOLYGON (((-76.02607 3...
#' $ COORDS_X <dbl> -71.25689, -75.79199, -78.55550, -84.32006, -87.27315, -99.32805, -96.37806, -106.44167, -11…
#' $ COORDS_Y <dbl> 44.34350, 42.57658, 39.16290, 33.49474, 40.79537, 32.96291, 40.02764, 43.68055, 37.02472, 45.06546
#' }
#' @examples
#' library(Rnssp)
#' data("hhs_sf")
#'
#' plot(hhs_sf$geometry)
"hhs_sf"
