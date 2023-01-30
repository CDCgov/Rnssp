#' A \code{NSSPContainer} Class to store a value in a container object
#'
#' @description
#' A \code{NSSPContainer} object stores a value
#'
#' @details
#' A \code{NSSPContainer} object can be used to retrieve a value
NSSPContainer <- R6::R6Class(
  "NSSPContainer",
  private = list(
    ..value = NULL
  ),
  active = list(
    value = function() {
      private$..value
    }
  ),
  public = list(

    #' @description
    #' Initializes a new NSSPContainer object.
    #' @param entry an object or value to be stored
    #' @return A new \code{NSSPContainer} object
    initialize = function(entry) {
      if (missing(entry)) {
        private$..value
      } else {
        private$..value <- entry
      }
    }
  )
)
