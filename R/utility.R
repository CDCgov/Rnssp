#' Password Prompt Utility
#'
#' Prompt the user for a password. This function is a wrapper for the \code{\link[askpass]{askpass}} function.
#'
#' @inheritParams askpass::askpass
#' @return a character string
#'
#' @seealso \code{\link[askpass]{askpass}}
#' @export
#'
#' @examples
#' password <- askme()
askme <- function(prompt = "Please enter your password: ") {
  askpass::askpass(prompt = prompt)
}


#' Utility to Add a New RMarkdown Template
#'
#' Add an NSSP RMarkdown template report to
#' extend an existing installation of the Rnssp package.
#'
#' @param pkg a character string with the name of a single package.
#' An error occurs if more than one package name is given.
#' @param template a character string with the name of a single template name.
#' The template name must be one of the elements of the vector returned
#' by \code{\link[Rnssp]{list_templates()}}.
#'
#' @details
#' In interactive mode, this utility function prompts the user to select an
#' Rmarkdown template .zip file when the \code{template} argument is not
#' specified or set to \code{NULL}.
#'
#' In non-interactive mode, this utility function prompts the user to specify a
#' path to an Rmarkdown template .zip file when the \code{template} argument
#' is not specified or set to \code{NULL}.
#'
#' When the \code{template} is specified, regardless of the \code{pkg} argument,
#' this utility function download the specified template from the
#' Rnssp-rmd-templates Github repository.
#' When the specified template name is not available, it throws an error.
#'
#' @return a character string
#' @seealso \href{https://github.com/CDCgov/Rnssp-rmd-templates}{https://github.com/CDCgov/Rnssp-rmd-templates}
#' @export
#'
#' @examples
#' \dontrun{
#' add_rmd_template() # Add a new Rmd template to the 'Rnssp' package
#' add_rmd_template(pkg = "rmarkdown") # Add a new Rmd template to the 'rmarkdown' package
#' add_rmd_template("text_mining") # Add the 'text_mining' template report to the Rnssp package
#' }
add_rmd_template <- function(template = NULL, pkg = "Rnssp") {
  if (is.null(template)) {
    if (!dir.exists(system.file(package = pkg))) {
      stop(paste0("The package '", pkg, "' is not installed!\n"))
    }
    zipfile <- NULL
    tryCatch(
      {
        filtre <- matrix(c("Template", ".zip"), 1, 2, byrow = TRUE)
        if (interactive() && .Platform$OS.type == "windows") {
          zipfile <- choose.files(filters = Filters[c("zip"), ])
        } else if (interactive() && .Platform$OS.type == "unix") {
          zipfile <- file.choose()
        } else if (!interactive()) {
          zipfile <- readline("Enter full path to template zip file: ")
        }
        if (!endsWith(zipfile, ".zip")) {
          cli::cli_abort("File provided is not a {.field .zip} file")
        }
        template_folder <- unlist(strsplit(basename(zipfile), "[.]"))[1]
        zipcontent <- unzip(zipfile, list = TRUE)
        stopifnot(exprs = {
          file.path(template_folder, "template.yaml") %in% zipcontent$Name
          file.path(template_folder, "skeleton", "skeleton.Rmd") %in% zipcontent$Name
        })
        exDir <- file.path(system.file(package = pkg), "rmarkdown/templates")
        unzip(zipfile, exdir = exDir)
        cli::cli({
          cli::cli_alert_success("Template {.field {template_folder}} has been successfully added in {.file {file.path(exDir, template_folder)}}.")
          cli::cli_alert_info("Please, restart your R session ({.kbd CTRL+SHIFT+F10} or {.kbd CMD+SHIFT+F10}) to update the template list!")
        })
      },
      error = function(e) {
        cli::cli_abort("No template added! The file provided is not a template zip file!")
      }
    )
  } else {
    repoURL <- "https://raw.githubusercontent.com/cdcgov/Rnssp-rmd-templates/master"
    template_list <- list_templates()
    if (!template %in% template_list) {
      cli::cli_abort("{.field {template}} is not a valid template.
                     Please run {.fn Rnssp::list_templates} to list available templates!")
    }
    temp_dir <- tempdir()
    zipfile <- file.path(temp_dir, paste0(template, ".zip"))
    download.file(file.path(repoURL, "zip", paste0(template, ".zip")),
      destfile = zipfile
    )
    if (!file.exists(zipfile)) {
      cli::cli_abort("Download of {.field {template}.zip} was unsuccessful!")
    }
    template_folder <- unlist(strsplit(basename(zipfile), "[.]"))[1]
    zipcontent <- unzip(zipfile, list = TRUE)
    stopifnot(exprs = {
      file.path(template_folder, "template.yaml") %in% zipcontent$Name
      file.path(template_folder, "skeleton", "skeleton.Rmd") %in% zipcontent$Name
    })
    exDir <- file.path(system.file(package = "Rnssp"), "rmarkdown/templates")
    if (!dir.exists(exDir)) {
      cli::cli_abort("Package {.pkg Rnssp} is not installed!")
    }
    unzip(zipfile, exdir = exDir)
    cli::cli({
      cli::cli_alert_success("Template {.field {template_folder}} has been successfully added in {.file {file.path(exDir, template_folder)}}.")
      cli::cli_alert_info("Please, restart your R session ({.kbd CTRL+SHIFT+F10} or {.kbd CMD+SHIFT+F10}) to update the template list!")
    })
  }
}


#' Utility to Remove an Existing RMarkdown Template
#'
#' Remove an Existing Rnssp Rmarkdown template directory.
#'
#' @param template a character string with the name of the template to delete
#' @param pkg a character string with the name of a single package.
#' An error occurs if more than one package name is given.
#' @param recursive logical. Should directories be deleted recursively? (Default is TRUE)
#' @param force logical. Should permissions be changed (if possible) to allow the file
#' or directory to be removed? (Default is TRUE)
#'
#' @return a character string
#' @export
#'
#' @examples
#' \dontrun{
#' remove_rmd_template("text_mining") # Remove the Existing Rnssp 'text_mining' template
#' }
remove_rmd_template <- function(template, pkg = "Rnssp", recursive = TRUE, force = TRUE) {
  if (!dir.exists(system.file(package = pkg))) {
    cli::cli_abort("The package {.pkg {pkg}} is not installed!")
  }
  if (!dir.exists(file.path(system.file(package = pkg), "rmarkdown/templates", template))) {
    cli::cli_abort("The template {.field {template}} does not exist for package {.pkg {pkg}}!")
  } else {
    unlink(file.path(system.file(package = pkg), "rmarkdown/templates", template), recursive = recursive, force = force)
    if (!dir.exists(file.path(system.file(package = pkg), "rmarkdown/templates", template))) {
      cli::cli({
        cli::cli_alert_success("Template {.field {template}} has been successfully removed from package {.pkg {pkg}}.")
        cli::cli_alert_info("Please, restart your R session ({.kbd CTRL+F10} or {.kbd CMD+F10}) to update the template list!")
      })
    }
  }
}


#' Utility to Change the Start and/Or End Dates in an ESSENCE API URL
#'
#' Change the start and/or end dates in an ESSENCE API URL
#'
#' @param url a character string of ESSENCE API URL
#' @param start_date a date object or a character string in date format (e.g. "2019-08-01")
#' @param end_date a date object or a character string in date format (e.g. "2020-08-01")
#'
#' @return a character string
#' @export
#'
#' @examples
#' library(Rnssp)
#'
#' url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/alerts/hospitalSyndromeAlerts?
#' end_date=31Jan2021&start_date=29Jan2021"
#'
#' url <- url %>% gsub("\n", "", .)
#'
#' # Change start date to January 15, 2021
#' url %>% change_dates(start = "2021-01-15")
#'
#' # Change end date to February 15, 2021
#' url %>% change_dates(end = "2021-02-15")
#'
#' # Change start date to January 15, 2021 and end date to February 15, 2021
#' url %>% change_dates(start = "2021-01-15", end = "2021-02-15")
#'
#' # Change end and start dates to respectively "current" and "current - 7 days"
#' url %>% change_dates(start = Sys.Date() - 7, end = Sys.Date())
change_dates <- function(url, start_date = NULL, end_date = NULL) {
  assertive.types::assert_is_a_string(url)
  prefixes <- list(
    epref = url %>%
      regmatches(., regexpr("endDate=\\d+[A-Za-z]+\\d+|end_date=\\d+[A-Za-z]+\\d+", .)) %>%
      str_split("=") %>%
      unlist() %>%
      .[1] %>%
      paste0(., "="),
    spref = url %>%
      regmatches(., regexpr("startDate=\\d+[A-Za-z]+\\d+|start_date=\\d+[A-Za-z]+\\d+", .)) %>%
      str_split("=") %>%
      unlist() %>%
      .[1] %>%
      paste0(., "=")
  )
  old_end <- url %>%
    regmatches(., regexpr("endDate=\\d+[A-Za-z]+\\d+|end_date=\\d+[A-Za-z]+\\d+", .)) %>%
    str_replace(., prefixes[["epref"]], "")
  old_start <- url %>%
    regmatches(., regexpr("startDate=\\d+[A-Za-z]+\\d+|start_date=\\d+[A-Za-z]+\\d+", .)) %>%
    str_replace(., prefixes[["spref"]], "")
  new_end <- old_end
  new_start <- old_start
  if (!is.null(end_date)) {
    new_end <- end_date %>%
      as.Date() %>%
      format(., "%e%b%Y") %>%
      str_trim()
  }
  if (!is.null(start_date)) {
    new_start <- start_date %>%
      as.Date() %>%
      format(., "%e%b%Y") %>%
      str_trim()
  }
  new_startd <- ifelse(nchar(new_start) > 7,
    as.Date(new_start, "%e%b%Y"),
    as.Date(new_start, "%e%b%y")
  )
  new_endd <- ifelse(nchar(new_end) > 7,
    as.Date(new_end, "%e%b%Y"),
    as.Date(new_end, "%e%b%y")
  )
  if (new_startd > new_endd) {
    cli::cli_abort("Start Date {.field {new_start}} is posterior to End Date {.field {new_end}}.")
  }
  str_replace(url, old_end, new_end) %>%
    str_replace(., old_start, new_start) %>%
    gsub("[[:space:]]", "", .)
}

#' Browse Rnssp vignettes
#'
#' Browse a specified Rnssp vignette, or browse the available ones
#' from the Rnssp online documentation.
#'
#' @param topic a character string giving the (base) name of the vignette to view.
#' If omitted, the webpage with all vignettes from the Rnssp package is browsed
#'
#' @return NULL
#' @seealso \code{\link[utils]{browseURL}}
#' @export
#'
#' @examples
#' \dontrun{
#' Rnssp_vignettes()
#' Rnssp_vignettes(topic = "Rnssp_intro")
#' }
Rnssp_vignettes <- function(topic = NULL) {
  base_url <- "https://cdcgov.github.io/Rnssp/articles"
  url <- base_url
  if (!is.null(topic)) {
    url <- file.path(base_url, paste0(topic, ".html"))
  }
  browseURL(url)
}


#' List Available Report Templates
#'
#' List available NSSP report templates from the Rnssp-rmd-templates Github repository.
#'
#' @param as.table a logical, if TRUE, a data frame is returned.
#' Otherwise, a vector is returned (Default is FALSE).
#'
#' @return A data frame or a vector
#' @seealso \href{https://github.com/CDCgov/Rnssp-rmd-templates}{https://github.com/CDCgov/Rnssp-rmd-templates}
#' @export
#'
#' @examples
#' list_templates()
#' list_templates(as.table = TRUE)
list_templates <- function(as.table = FALSE) {
  req <- httr::GET("https://api.github.com/repos/cdcgov/Rnssp-rmd-templates/git/trees/master?recursive=1")
  repoURL <- "https://raw.githubusercontent.com/cdcgov/Rnssp-rmd-templates/master"
  httr::stop_for_status(req)
  filelist <- unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = F)
  templates <- unique(dirname(filelist[grepl("/skeleton$", filelist)]))
  if (as.table) {
    do.call(
      rbind.data.frame,
      lapply(
        templates,
        function(template) {
          tibble::add_column(
            tibble::as_tibble(
              yaml::read_yaml(
                file.path(repoURL, template, "template.yaml")
              )
            ),
            .before = 1,
            id = template
          )
        }
      )
    )
  } else {
    templates
  }
}
