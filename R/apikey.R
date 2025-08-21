#' An \code{Apikey} Class Representing an Apikey object
#'
#' @description
#' A \code{Apikey} object has an api_key string and a key name string.
#'
#' @details
#' An \code{Apikey} object can get API data via an API URL.

Apikey <- R6::R6Class(
  "NSSPApikey",
  inherit = Auth,
  private = list(
    ..api_key = NSSPContainer$new(NULL),
    ..key_name = NULL
  ),
  public = list(

    #' @description
    #' Initializes a new Credentials object.
    #' @param api_key a string for API key
    #' @param key_name a string for an API Key name. Default is \code{API-KEY}
    #' @return An object of class \code{Apikey}
    initialize = function(api_key, key_name = "API-KEY") {
      if (!missing(api_key)) {
        private$..api_key <- NSSPContainer$new(safer::encrypt_string(api_key, key = private$..__$value, ascii = FALSE))
        private$..key_name <- key_name
      }
    },

    #' @description
    #' Get API response
    #' @param url a character of API URL
    #' @return An object of class \code{response}
    #' @examples
    #' \dontrun{
    #' myProfile <- Apikey$new("abc1234567890")
    #' url <- "https://httpbin.org/json"
    #' api_response <- myProfile$get_api_response(url)
    #' }
    get_api_response = function(url) {
      if (is.null(private$..api_key$value)) {
        message("Please, set your API key!")
      } else {
        assertions::assert_string(url)
        headers <- list()
        headers[[private$..key_name]] = private$..api_key$value %>%
          safer::decrypt_string(., private$..__$value)
        res <- url %>%
          httr::GET(., httr::add_headers(unlist(headers)))
        headers <- NULL
        res$request$headers <- NULL
        cli::cli_alert_info(httr::http_status(res$status_code)$message)
        return(res)
      }
    },

    #' @description
    #' Get API graph
    #' @param url a character of API URL
    #' @param file_ext a non-empty character vector giving the file extension. Default is \code{.png}.
    #' @return A list containing an api_response object and a path to a time series graph in .png format
    #' @examples
    #' \dontrun{
    #' myProfile <- Apikey$new("abc1234567890")
    #' url <- "https://httpbin.org/image/png"
    #' api_data_graph <- myProfile$get_api_graph(url)
    #' names(api_data_graph)
    #' img <- png::readPNG(api_data_graph$graph)
    #' grid::grid.raster(img)
    #' }
    get_api_graph = function(url, file_ext = ".png") {
      assertions::assert_string(url)
      graph <- tempfile(fileext = file_ext)
      headers <- list()
      headers[[private$..key_name]] = private$..api_key$value %>%
        safer::decrypt_string(., private$..__$value)
      apir <- url %>%
        httr::GET(., httr::add_headers(unlist(headers)),
                  httr::write_disk(graph, overwrite = TRUE))
      apir$request$headers <- NULL
      headers <- NULL
      cli::cli_alert_info(httr::http_status(apir$status_code)$message)
      list("api_response" = apir, "graph" = graph)
    }
  )
)
