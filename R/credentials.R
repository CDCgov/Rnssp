#' A `NSSPCredentials` Class Representing a Credentials object
#'
#' @description
#' A `Credentials` object has a username, a password and a key.
#'
#' @details
#' A `Credentials` object can get ESSENCE API data via an API URL.

Credentials <- R6::R6Class(
  "NSSPCredentials",
  private = list(
    ..username = NSSPContainer$new(NULL),
    ..password = NSSPContainer$new(NULL),
    ..__ = NSSPContainer$new(stringi::stri_rand_strings(1, 1024, pattern = "[A-Za-z0-9]"))
  ),
  public = list(

    #' @description
    #' Initializes a new Credentials object.
    #' @param username a character of ESSENCE username
    #' @param password a character of ESSENCE password
    #' @return A new `Credentials` object
    initialize = function(username, password) {
      if (!missing(username)) {
        private$..username <- NSSPContainer$new(safer::encrypt_string(username, key = private$..__$value, ascii = FALSE))
      }
      if (!missing(password)) {
        private$..password <- NSSPContainer$new(safer::encrypt_string(password, key = private$..__$value, ascii = FALSE))
      }
      # if (!missing(key)) {
      #   private$..__ <- NSSPContainer$new(key)
      # }
    },

    #' @description
    #' Get ESSENCE API response
    #' @param url a character of ESSENCE API URL
    #' @return An object of class response
    #' @examples
    #' \dontrun{
    #' myProfile <- Credentials$new(askme("Enter my username: "), askme())
    #' url <- "<ESSENCE_url>"
    #' api_response <- myProfile$get_api_response(url)
    #' }
    get_api_response = function(url) {
      if (is.null(private$..username$value) | is.null(private$..password$value)) {
        message("Please, set your ESSENCE credentials (username and password)!")
      } else {
        assertive.types::assert_is_a_string(url)
        url %>%
          httr::GET(., httr::authenticate(
            private$..username$value %>% safer::decrypt_string(., private$..__$value),
            private$..password$value %>% safer::decrypt_string(., private$..__$value)
          ))
      }
    },

    #' @description
    #' Get ESSENCE API data
    #' @param url a character of ESSENCE API URL
    #' @param fromCSV a logical, defines whether data are returned in .csv format or .json format
    #' @return A dataframe (`fromCSV = TRUE`) or a list containing a dataframe and its metadata (`fromCSV = FALSE`)
    #' @examples
    #' \dontrun{
    #' myProfile <- Credentials$new(askme("Enter my username: "), askme())
    #' json_url <- "<json type ESSENCE_url>"
    #' api_data_json <- myProfile$get_api_data(json_url)
    #'
    #' csv_url <- "<csv type ESSENCE_url>"
    #' api_data_csv <- myProfile$get_api_data(csv_url, fromCSV = TRUE)
    #' }
    get_api_data = function(url, fromCSV = FALSE) {
      assertive.types::assert_is_a_string(url)
      self$get_api_response(url) %>% {
        if (fromCSV) {
          httr::content(., by = "text/csv") %>% readr::read_csv()
        } else {
          httr::content(., as = "text") %>% jsonlite::fromJSON()
        }
      }
    },

    #' @description
    #' Get ESSENCE API data
    #' @param url a character of ESSENCE API URL
    #' @return A list containing an api_response object and a path to a time series graph in .png format
    #' @examples
    #' \dontrun{
    #' myProfile <- Credentials$new(askme("Enter my username: "), askme())
    #' url <- "<ESSENCE_url>"
    #' api_data_tsgraph <- myProfile$get_api_tsgraph(url)
    #' names(api_data_tsgraph)
    #' img <- png::readPNG(api_data_tsgraph$tsgraph)
    #' grid::grid.raster(img)
    #' }
    get_api_tsgraph = function(url) {
      tsgraph <- tempfile(fileext = ".png")
      apir <- url %>%
        httr::GET(., httr::authenticate(
          private$..username$value %>% safer::decrypt_string(., private$..__$value),
          private$..password$value %>% safer::decrypt_string(., private$..__$value)
        ), httr::write_disk(tsgraph, overwrite = TRUE))
      list("api_response" = apir, "tsgraph" = tsgraph)
    }
  )
)
