#' @keywords internal
"_PACKAGE"
#' Tools, Functions, Shapefiles data, and Rmarkdown templates to work with Syndromic Surveillance data
#'
#' The National Syndromic Surveillance Program (NSSP) at the Centers for Disease and Control has a large Community of Practice (CoP)
#' including states and local Departments of Health. The NSSP has developed multiple resources including analysis reports,
#' analytic tools, and assisted with scripts to interface with ESSENCE. The \code{Rnssp} R package is built in an attempt to
#' improve reusability, reproducibility, and distribution of the analytical resources developed at the CDC NSSP.
#'
#' @section Rnssp functions:
#' The Rnssp functions include:
#' \itemize{
#'     \item API functions to pull data from the ESSENCE system or any REST API service
#'     \item Various utility functions to securely set user credentials, add or remove R Markdown templates, or set new dates in API urls.
#'     \item Anomaly detection and Time Series Trend classification algorithms
#'     \item A feature to extend the package installation with NSSP custom-built parameterized R Markdown templates
#'     \item Utilities to run custom-built shiny apps provided by NSSP
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
#' @section Rnssp Shiny Applications:
#' The Rnssp package comes with one custom-built Rnssp Shiny application.
#' Users may run Rnssp custom-built shiny applications via Rnssp Rstudio addins
#' or via the \code{\link[Rnssp]{run_app()}} utility function.
#' To list all available Rnssp shiny apps, run the \code{\link[Rnnsp]{list_apps()}} function.
#' Please, check the Rnssp-shiny-appse Github repository at
#' \href{https://github.com/cdcgov/rnssp-shiny-apps}{https://github.com/cdcgov/rnssp-shiny-apps}.
#'
#' @docType package
#' @name Rnssp
#' @author Gbedegnon Roseric Azondekon (\email{gazondekon@cdc.gov})
#' @import dplyr, readr, magrittr, R6, stringi, assertions, safer, jsonlite, httr, askpass, yaml
#' @import sf, shiny (>= 0.13), miniUI (>= 0.1.1), rstudioapi (>= 0.5)
NULL
# > NULL
