#' A \code{Token} Class Representing a Token object
#'
#' @description
#' A \code{Token} object has a token string and an authentication type.
#'
#' @details
#' A \code{Token} object can get API data via an API URL.

Token <- R6::R6Class(
  "NSSPToken",
  inherit = Auth,
  private = list(
    ..token = NSSPContainer$new(NULL),
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
