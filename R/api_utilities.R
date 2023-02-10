#' Create a profile with a username and password
#'
#' A wrapper to the \code{new} method defined in the \code{\link[Rnssp]{Credentials}} class.
#'
#' @param username a string for username
#' @param password a string for password
#'
#' @return an object of class NSSPCredentials and R6
#' @export
#'
#' @examples
#' \dontrun{
#' myProfile <- create_profile()
#' myProfile
#' }
#'
create_profile <- function(
  username = askme("Please enter your username: "),
  password = askme()
){
  Credentials$new(username = username, password = password)
}


#' Create a profile with an API token
#'
#' A wrapper to the \code{new} method defined in the \code{\link[Rnssp]{Token}} class.
#'
#' @param token a string for token
#' @param auth_type type of HTTP authentication.
#'        Should be \code{Bearer} or \code{Basic}. Default is \code{Bearer}
#'
#' @return An object of class \code{Token}
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a profile with "Bearer" authentication type
#' myProfile <- create_token_profile()
#' myProfile
#'
#' # Create a profile with a "Basic" authentication type
#' myProfile2 <- create_token_profile(auth_type = "Basic")
#' myProfile2
#' }
#'
create_token_profile <- function(
    token = askpass:::readline_silent("Enter/Paste a token: "),
    auth_type = "Bearer"
){
  Token$new(token = token, auth_type = auth_type)
}

#' Get API data
#'
#' A wrapper to the \code{get_api_data} method defined in the
#' \code{\link[Rnssp]{Credentials}} class.
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
#' myProfile <- create_profile("", "")
#'
#' json_url <- "http://httpbin.org/json"
#' api_data_json <- get_api_data(json_url)
#' head(api_data_json$slideshow$slides)
#'
#' csv_url <- "http://httpbin.org/robots.txt"
#' api_data_csv <- get_api_data(csv_url, fromCSV = TRUE)
#' head(api_data_csv)
#' }
get_api_data <- function(url, fromCSV = FALSE, profile = myProfile, ...) {
  profile$get_api_data(url = url, fromCSV = fromCSV, ...)
}


#' Get API response
#'
#' A wrapper to the \code{get_api_response} method defined in the
#' \code{\link[Rnssp]{Credentials}} class.
#'
#' @param url a character of API URL
#' @param profile an object of class \code{\link[Rnssp]{Credentials}}.
#'     Default is \code{myProfile}.
#'
#' @seealso \code{\link[Rnssp]{Credentials}}
#' @return An object of class \code{response}.
#' @export
#'
#' @examples
#' \dontrun{
#' myProfile <- create_profile("", "")
#' url <- "http://httpbin.org/json"
#'
#' api_response <- get_api_response(url)
#' names(api_response)
#' }
get_api_response <- function(url, profile = myProfile) {
  profile$get_api_response(url = url)
}


#' Get an API graph
#'
#' A wrapper to the \code{get_api_tsgraph} method defined in the
#' \code{\link[Rnssp]{Credentials}} class.
#'
#' @param url a character of API URL.
#' @param profile an object of class \code{\link[Rnssp]{Credentials}}.
#'     Default is \code{myProfile}.
#'
#' @seealso \code{\link[Rnssp]{Credentials}}
#' @return An object of class \code{response}.
#' @export
#'
#' @examples
#' \dontrun{
#' myProfile <- create_profile("", "")
#' url <- "http://httpbin.org/image/png"
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

#' Get an API graph
#'
#' A wrapper to the \code{get_api_graph} method defined in the
#' \code{\link[Rnssp]{Credentials}} class.
#'
#' @param url a character of API URL.
#' @param file_ext a non-empty character vector giving the file extension.
#'     Default is \code{.png}.
#' @param profile an object of class \code{\link[Rnssp]{Credentials}}.
#'     Default is \code{myProfile}.
#'
#' @seealso \code{\link[Rnssp]{Credentials}}
#' @return An object of class \code{response}.
#' @export
#'
#' @examples
#' \dontrun{
#' myProfile <- create_profile("", "")
#' url <- "http://httpbin.org/image/png"
#'
#' api_data_graph <- get_api_graph(url)
#'
#' names(api_data_graph)
#' img <- png::readPNG(api_data_graph$graph)
#' grid::grid.raster(img)
#' }
get_api_graph <- function(url, file_ext = ".png", profile = myProfile) {
  if(!"get_api_graph" %in% names(profile)){
    cli::cli_alert_info("Outdated profile detected!")
    cli::cli_abort("Please create a new profile or use
                   {.fn get_api_tsgraph} instead!")
  }
  profile$get_api_graph(url = url, file_ext = file_ext)
}

#' Get ESSENCE data
#'
#' Get ESSENCE API data.
#'
#' @param url a character of ESSENCE API URL.
#' @param start_date a date object or a character string in date
#'     format (e.g. "2019-08-01")
#' @param end_date a date object or a character string in date format
#'     (e.g. "2020-08-01")
#' @param profile an object of class \code{\link[Rnssp]{Credentials}}.
#'     Default is \code{myProfile}.
#' @param ... further arguments and CSV parsing parameters to be passed to
#'     \code{\link[readr]{read_csv}} when \code{fromCSV = TRUE}.
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
get_essence_data <- function(url, start_date = NULL,
                             end_date = NULL, profile = myProfile, ...) {
  api_type <- str_extract(url, "(?<=api/).+(?=\\?)")

  url_new <- try(change_dates(url, start_date, end_date), silent = TRUE)

  if(any(class(url_new) == "try-error")){
    cli::cli_abort("URL is not of ESSENCE type. Check your URL or use
                   {.fn get_api_data} instead!")
  }

  switch(
    api_type,
    "timeSeries" = profile$get_api_data(url_new) %>%
      extract2("timeSeriesData"),
    "timeSeries/graph" = profile$get_api_graph(url_new) %>%
      extract2("graph"),
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
    cli::cli_abort("URL is not of ESSENCE type. Check your URL or use
                   {.fn get_api_data} instead!")
  )
}
