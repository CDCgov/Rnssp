#' US Health and Human Services Regions Shapefile
#'
#' Useful `sf`` object shapefile for US Health and Human Services (HHS) Regions.
#'
#' \preformatted{
#' Rows: 12
#' Columns: 2
#' $ regions  <dbl> 2, 1, 6, 7, 3, 8, 5, 10, 10, 4, 9, 9
#' $ geometry <MULTIPOLYGON [m]> MULTIPOLYGON (((1665094 178..., MULTIPOLYGON (((1663772 239..., MULTIPOLYGON (((-507864 126..., MULTIP...
#' }
#' @examples
#' library(Rnssp)
#' data("hhs_sf")
#'
#' plot(hhs_sf)
"hhs_sf"
