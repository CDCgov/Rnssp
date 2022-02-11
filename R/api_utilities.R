#' Get ESSENCE API data
#'
#' A wrapper to the \code{get_api_data} method defined in the \code{\link[Rnssp]{Credentials}} class.
#'
#' @param url a character of ESSENCE API URL
#' @param fromCSV a logical, defines whether data are returned in .csv format or .json format
#' @param profile an object of class \code{\link[Rnssp]{Credentials}}. Default is \code{myProfile}.
#' @param ... further arguments and CSV parsing parameters to be passed to \code{\link[readr]{read_csv}} when \code{fromCSV = TRUE}.
#'
#' @seealso \code{\link[Rnssp]{Credentials}}
#' @return a dataframe (\code{fromCSV = TRUE}) or a list containing a dataframe and its metadata (\code{fromCSV = TRUE})
#' @export
#'
#' @examples
#' \dontrun{
#' myProfile <- Credentials$new(askme("Enter my username: "), askme())
#'
#' json_url <- "<json type ESSENCE_url>"
#' api_data_json <- get_api_data(json_url)
#'
#' csv_url <- "<csv type ESSENCE_url>"
#' api_data_csv <- get_api_data(csv_url, fromCSV = TRUE)
#' }
get_api_data <- function(url, fromCSV = FALSE, profile = myProfile,  ...) {
  profile$get_api_data(url = url, fromCSV = fromCSV, ...)
}


#' Get ESSENCE API response
#'
#' A wrapper to the \code{get_api_response} method defined in the \code{\link[Rnssp]{Credentials}} class.
#'
#' @param url a character of ESSENCE API URL
#' @param profile an object of class \code{\link[Rnssp]{Credentials}}. Default is \code{myProfile}.
#'
#' @seealso \code{\link[Rnssp]{Credentials}}
#' @return An object of class \code{response}
#' @export
#'
#' @examples
#' \dontrun{
#' myProfile <- Credentials$new(askme("Enter my username: "), askme())
#' url <- "<ESSENCE_url>"
#'
#' api_response <- get_api_response(url)
#' }
get_api_response <- function(url, profile = myProfile) {
  profile$get_api_response(url = url)
}


#' Get ESSENCE API Time Series Graph
#'
#' A wrapper to the \code{get_api_tsgraph} method defined in the \code{\link[Rnssp]{Credentials}} class.
#'
#' @param url a character of ESSENCE API URL
#' @param profile an object of class \code{\link[Rnssp]{Credentials}}. Default is \code{myProfile}.
#'
#' @seealso \code{\link[Rnssp]{Credentials}}
#' @return An object of class \code{response}
#' @export
#'
#' @examples
#'  \dontrun{
#' myProfile <- Credentials$new(askme("Enter my username: "), askme())
#' url <- "<ESSENCE_url>"
#'
#' api_data_tsgraph <- get_api_tsgraph(url)
#'
#' names(api_data_tsgraph)
#' img <- png::readPNG(api_data_tsgraph$tsgraph)
#' grid::grid.raster(img)
#' }
get_api_tsgraph <- function(url, profile = myProfile) {
  profile$get_api_response(url = url)
}
