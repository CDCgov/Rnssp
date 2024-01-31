#' A \code{Token} Class Representing a Token object
#'
#' @description
#' A \code{Token} object has a token string and a key.
#'
#' @details
#' A \code{Token} object can get API data via an API URL.

Token <- R6::R6Class(
  "NSSPToken",
  private = list(
    ..token = NSSPContainer$new(NULL),
    ..__ = NSSPContainer$new(stringi::stri_rand_strings(1, 1024, pattern = "[A-Za-z0-9*-+=/_$@.?!%|;:#~<>()[]\`\']")),
    ..auth_type = NULL
  ),
  public = list(

    #' @description
    #' Initializes a new Credentials object.
    #' @param token a string for token
    #' @param auth_type type of HTTP authentication. Should be \code{Bearer} or \code{Basic}. Default is \code{Bearer}
    #' @return An object of class \code{Token}
    initialize = function(token, auth_type = "Bearer") {
      if (!missing(token)) {
        private$..token <- NSSPContainer$new(safer::encrypt_string(token, key = private$..__$value, ascii = FALSE))
        private$..auth_type <- auth_type
      }
    },

    #' @description
    #' Get API response
    #' @param url a character of API URL
    #' @return An object of class \code{response}
    #' @examples
    #' \dontrun{
    #' myProfile <- Token$new("abc1234567890")
    #' url <- "https://httpbin.org/json"
    #' api_response <- myProfile$get_api_response(url)
    #' }
    get_api_response = function(url) {
      if (is.null(private$..token$value)) {
        message("Please, set your token!")
      } else {
        assertions::assert_string(url)
        res <- url %>%
          httr::GET(., httr::add_headers(
            Authorization = paste(
              private$..auth_type,
              private$..token$value %>%
                safer::decrypt_string(., private$..__$value)
            )
          ))
        res$request$options$auth_token <- ""
        cli::cli_alert_info(httr::http_status(res$status_code)$message)
        return(res)
      }
    },

    #' @description
    #' Get API data
    #' @param url a character ofAPI URL
    #' @param fromCSV a logical, defines whether data are returned in .csv format or .json format
    #' @param ... further arguments and CSV parsing parameters to be passed to \code{\link[readr]{read_csv}} when \code{fromCSV = TRUE}.
    #' @return a dataframe (\code{fromCSV = TRUE}) or a list containing a dataframe and its metadata (\code{fromCSV = TRUE})
    #' @examples
    #' \dontrun{
    #' myProfile <- Token$new("abc1234567890")
    #' json_url <- "https://httpbin.org/json"
    #' api_data_json <- myProfile$get_api_data(json_url)
    #'
    #' csv_url <- "https://httpbin.org/robots.txt"
    #' api_data_csv <- myProfile$get_api_data(csv_url, fromCSV = TRUE)
    #' }
    get_api_data = function(url, fromCSV = FALSE, ...) {
      assertions::assert_string(url)
      apir <- self$get_api_response(url)
      if(apir$status_code == 200){
        apir %>% {
          if (fromCSV) {
            httr::content(., by = "text/csv") %>% readr::read_csv(...)
          } else {
            httr::content(., as = "text") %>% jsonlite::fromJSON()
          }
        }
      }
    },

    #' @description
    #' Get API graph
    #' @param url a character of API URL
    #' @param file_ext a non-empty character vector giving the file extension. Default is \code{.png}.
    #' @return A list containing an api_response object and a path to a time series graph in .png format
    #' @examples
    #' \dontrun{
    #' myProfile <- Token$new("abc1234567890")
    #' url <- "https://httpbin.org/image/png"
    #' api_data_graph <- myProfile$get_api_graph(url)
    #' names(api_data_graph)
    #' img <- png::readPNG(api_data_graph$graph)
    #' grid::grid.raster(img)
    #' }
    get_api_graph = function(url, file_ext = ".png") {
      assertions::assert_string(url)
      graph <- tempfile(fileext = file_ext)
      apir <- url %>%
        httr::GET(., httr::add_headers(
          Authorization = paste(
            private$..auth_type,
            private$..token$value %>%
              safer::decrypt_string(., private$..__$value)
          )
        ), httr::write_disk(graph, overwrite = TRUE))
      apir$request$options$auth_token <- ""
      cli::cli_alert_info(httr::http_status(apir$status_code)$message)
      list("api_response" = apir, "graph" = graph)
    }
  )
)
