#' @keywords internal
"_PACKAGE"
#' Tools, Functions, Shapefiles data, and Rmarkdown templates to work with Syndromic Surveillance data via NSSP ESSENCE
#'
#' The National Syndromic and Surveillance Program (NSSP) at the Centers for Disease and Control has a large Community of Practice (CoP)
#' including state and local Departments of Health. The NSSP has developed multiple resources including analysis reports,
#' analytic tools, and assisted with scripts to interface with ESSENCE. The \code{Rnssp} R package is built in an attempt to
#' improve reusability, reproducibility, and distribution of the analytical resources developed at the CDC NSSP.
#'
#' @section Rnssp functions:
#' The Rnssp functions include:
#' \itemize{
#'     \item API functions to pull data from the ESSENCE system
#'     \item Various utility functions to securely set user credentials, add or remove R Markdown templates, or set new dates in API urls.
#'     \item Anomaly detection and Time Series Trend classification algorithms
#' }
#'
#' @section Rnssp vignettes:
#' The vignettes serve as instructive tutorials demonstrating practical uses of the
#' Rnssp package. Please run the \code{\link[Rnnsp]{Rnssp_vignettes()}} function to browse all available vignettes.
#'
#' @section Rnssp Rmarkdown templates:
#' By default, the Rnssp package comes with one R Markdown template. We leave it up
#' to users to decide which template to extend the package with post-installation.
#' To list all available template, run the \code{\link[Rnnsp]{list_templates()}} function. Please, check
#' the Rnssp R markdown template Github repository at
#' \href{https://github.com/cdcgov/rnssp-rmd-templates}{https://github.com/cdcgov/rnssp-rmd-templates}.
#'
#' @docType package
#' @name Rnssp
#' @author Gbedegnon Roseric Azondekon (\href{mailto:gazondekon@cdc.gov}{gazondekon@cdc.gov})
#' @import dplyr, readr, magrittr, R6, stringi, assertive.types, safer, jsonlite, httr, askpass, yaml
#' @import sf
NULL
# > NULL
