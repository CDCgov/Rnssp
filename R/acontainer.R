#' A \code{NSSPContainer} Class to store a value or an object
#'
#' @description
#' An object of class \code{NSSPContainer} stores a value
#'
#' @details
#' The \code{NSSPContainer} class is used to encapsulate a value or an object
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
