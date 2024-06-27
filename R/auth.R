#' An \code{Auth} Class is an abstract authentication class
#'
#' @description
#' An \code{Auth} has a private key attribute.

Auth <- R6::R6Class(
  "NSSPAuth",
  private = list(
    ..__ = NSSPContainer$new(stringi::stri_rand_strings(1, 1024, pattern = "[A-Za-z0-9*-+=/_$@.?!%|;:#~<>()[]\`\']"))
  ),
  public = list(

    #' @description
    #' Get API response
    #' @param url a character of API URL
    #' @return An object of class \code{response}
    #' @examples
    #' \dontrun{
    #' myAuth <- Auth$new()
    #' url <- "https://httpbin.org/json"
    #' api_response <- myAuth$get_api_response(url)
    #' }
    get_api_response = function(url) {
    },

    #' @description
    #' Get API data
    #' @param url a character ofAPI URL
    #' @param fromCSV a logical, defines whether data are returned in .csv format or .json format
    #' @param ... further arguments and CSV parsing parameters to be passed to \code{\link[readr]{read_csv}} when \code{fromCSV = TRUE}.
    #' @return a dataframe (\code{fromCSV = TRUE}) or a list containing a dataframe and its metadata (\code{fromCSV = TRUE})
    #' @examples
    #' \dontrun{
    #' myAuth <- Auth$new()
    #' json_url <- "https://httpbin.org/json"
    #' api_data_json <- myAuth$get_api_data(json_url)
    #'
    #' csv_url <- "https://httpbin.org/robots.txt"
    #' api_data_csv <- myAuth$get_api_data(csv_url, fromCSV = TRUE)
    #' }
    get_api_data = function(url, fromCSV = FALSE, ...) {
      assertions::assert_string(url)
      apir <- self$get_api_response(url)
      if(apir$status_code == 200){
        if(any("data.frame" %in% class(httr::content(apir, as = "text")))){
          return(httr::content(apir, as = "text"))
        }
        apir %>% {
          if (fromCSV) {
            httr::content(., by = "text/csv") %>% readr::read_csv(...)
          } else {
            httr::content(., as = "text") %>% jsonlite::fromJSON()
          }
        }
      }
    }
  )
)
