#' A wrapper to the \code{new} method defined in the \code{\link[Rnssp]{Credentials}} class.
#'
#' @param username a string for username
#' @param password a string for password
#'
#' @return an object of class NSSPCredentials and R6
#' @export
#'
#' @examples
create_profile <- function(
  username = askme("Please enter your username: "),
  password = askme()
){
  Credentials$new(username = username, password = password)
}

#' Get API data
#'
#' A wrapper to the \code{get_api_data} method defined in the \code{\link[Rnssp]{Credentials}} class.
#'
#' @param url a character of API URL.
#' @param fromCSV a logical, defines whether data are returned in .csv format or .json format.
#' @param profile an object of class \code{\link[Rnssp]{Credentials}}. Default is \code{myProfile}.
#' @param ... further arguments and CSV parsing parameters to be passed to \code{\link[readr]{read_csv}} when \code{fromCSV = TRUE}.
#'
#' @seealso \code{\link[Rnssp]{Credentials}}
#' @return a dataframe (\code{fromCSV = TRUE}) or a list containing a dataframe and its metadata (\code{fromCSV = TRUE}).
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
get_api_data <- function(url, fromCSV = FALSE, profile = myProfile, ...) {
  profile$get_api_data(url = url, fromCSV = fromCSV, ...)
}


#' Get API response
#'
#' A wrapper to the \code{get_api_response} method defined in the \code{\link[Rnssp]{Credentials}} class.
#'
#' @param url a character of API URL
#' @param profile an object of class \code{\link[Rnssp]{Credentials}}. Default is \code{myProfile}.
#'
#' @seealso \code{\link[Rnssp]{Credentials}}
#' @return An object of class \code{response}.
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


#' Get API Time Series Graph
#'
#' A wrapper to the \code{get_api_tsgraph} method defined in the \code{\link[Rnssp]{Credentials}} class.
#'
#' @param url a character of API URL.
#' @param profile an object of class \code{\link[Rnssp]{Credentials}}. Default is \code{myProfile}.
#'
#' @seealso \code{\link[Rnssp]{Credentials}}
#' @return An object of class \code{response}.
#' @export
#'
#' @examples
#' \dontrun{
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
  profile$get_api_tsgraph(url = url)
}

#' Get API data
#'
#' Get API data.
#'
#' @param url a character of API URL.
#' @param start_date a date object or a character string in date format (e.g. "2019-08-01")
#' @param end_date a date object or a character string in date format (e.g. "2020-08-01")
#' @param profile an object of class \code{\link[Rnssp]{Credentials}}. Default is \code{myProfile}.
#' @param ... further arguments and CSV parsing parameters to be passed to \code{\link[readr]{read_csv}} when \code{fromCSV = TRUE}.
#'
#' @return a dataframe or a character string.
#' @seealso \code{\link[Rnssp]{get_api_data}} and \code{\link[Rnssp]{get_api_tsgraph}}
#' @export
#'
#' @examples
#'
#' \dontrun{
#' myProfile <- create_profile(askme("Enter your username:"), askme())
#'
#' # Example 1
#' url <- "https://essence2.syndromicsurveillance.org/nssp_essence/api/timeSeries?
#' endDate=20Sep22&ccddCategory=cli%20cc%20with%20cli%20dd%20and%20coronavirus%20dd%20v2
#' &percentParam=ccddCategory&geographySystem=hospitaldhhsregion&datasource=va_hospdreg
#' &detector=probrepswitch&startDate=22Jun22&timeResolution=daily&hasBeenE=1
#' &medicalGroupingSystem=essencesyndromes"
#'
#' url <- url %>% gsub("\n", "", .)
#'
#' ## Pull ESSENCE data
#' api_data <- get_essence_data(url)
#'
#' ## Inspect data
#' glimpse(api_data)
#'
#'
#' # Example 2
#' url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/alerts/regionSyndromeAlerts?
#' end_date=31Jan2021&start_date=29Jan2021"
#'
#' url <- url %>% gsub("\n", "", .)
#'
#' ## Pull last 30 days of ESSENCE data
#' api_data <- get_essence_data(url, start_date = Sys.Date() - 30, end_date = Sys.Date())
#'
#' ## Inspect data
#' glimpse(api_data)
#'
#'
#' # Example 3
#' url <- "https://essence2.syndromicsurveillance.org/nssp_essence/api/timeSeries/graph?
#' endDate=25Jun2022&geography=&percentParam=noPercent&datasource=va_hosp&startDate=25Jun2021
#' &medicalGroupingSystem=essencesyndromes&userId=3751&aqtTarget=TimeSeries&ccddCategory=
#' &geographySystem=hospitalregion&detector=probrepswitch&timeResolution=daily"
#'
#' url <- url %>% gsub("\n", "", .)
#'
#' ## Pull Time series graph
#' api_tsgraph <- get_essence_data(url)
#'
#' ## Preview time series graph
#' graph <- imager::load.image(api_tsgraph)
#' plot(graph)
#' }
#'
get_essence_data <- function(url, start_date = NULL, end_date = NULL, profile = myProfile, ...) {
  api_type <- str_extract(url, "(?<=api/).+(?=\\?)")

  url_new <- try(change_dates(url, start_date, end_date), silent = TRUE)

  if(any(class(url_new) == "try-error")){
    cli::cli_abort("URL is not of ESSENCE type. Check your URL or use {.fn get_api_data} instead!")
  }

  switch(
    api_type,
    "timeSeries" = profile$get_api_data(url_new) %>%
      extract2("timeSeriesData"),
    "timeSeries/graph" = profile$get_api_tsgraph(url_new) %>%
      extract2("tsgraph"),
    "tableBuilder/csv" = profile$get_api_data(url_new, fromCSV = TRUE, ...),
    "dataDetails" = profile$get_api_data(url_new) %>%
      extract2("dataDetails"),
    "dataDetails/csv" = profile$get_api_data(url_new, fromCSV = TRUE, ...),
    "summaryData" = profile$get_api_data(url_new) %>%
      extract2("summaryData"),
    "alerts/regionSyndromeAlerts" = myProfile$get_api_data(url_new) %>%
      extract2("regionSyndromeAlerts"),
    "alerts/hospitalSyndromeAlerts" = profile$get_api_data(url_new) %>%
      extract2("hospitalSyndromeAlerts"),
    cli::cli_abort("URL is not of ESSENCE type. Check your URL or use {.fn get_api_data} instead!")
  )
}
