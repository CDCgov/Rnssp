#' Password Prompt Utility
#'
#' Prompt the user for a password. This function is a wrapper for the `askpass::askpass()` function.
#'
#' @inheritParams askpass::askpass
#' @return a character string
#'
#' @seealso \code{\link[askpass]{askpass}}
#' @export
#'
#' @examples
#' password <- askme()
askme <- function(prompt = "Please enter your password: "){
  askpass::askpass(prompt = prompt)
}


#' Prompt Utility to Add a New RMarkdown Template
#'
#' Prompt the user to select an Rmarkdown template .zip file.
#'
#' @param pkg a character string with the name of a single package. An error occurs if more than one package name is given.
#'
#' @return a character string
#' @export
#'
#' @examples
#' \dontrun{
#' add_rmd_template() # Add a new Rmd template to the 'Rnssp' package
#' add_rmd_template("rmarkdown") # Add a new Rmd template to the 'rmarkdown' package
#' }
add_rmd_template <- function(pkg = "Rnssp") {
  if(!dir.exists(system.file(package = pkg))){
    stop(paste0("The package '", pkg, "' is not installed!\n"))
  }
  zipfile <- NULL
  tryCatch({
    filtre <- matrix(c("Template", ".zip"), 1, 2, byrow = TRUE)
    if (interactive() && .Platform$OS.type == "windows"){
      zipfile <- choose.files(filters = Filters[c("zip"),])
    } else if (interactive() && .Platform$OS.type == "unix"){
      zipfile <- file.choose()
    } else if(!interactive()){
      zipfile <- readline("Enter full path to template zip file: ")
    }
    if(!endsWith(zipfile, ".zip")){
      stop("File provided is not a .zip file")
    }
    template_folder <- unlist(strsplit(basename(zipfile), '[.]'))[1]
    zipcontent <- unzip(zipfile, list = TRUE)
    stopifnot(exprs = {
      file.path(template_folder, "template.yaml") %in% zipcontent$Name
      file.path(template_folder, "skeleton", "skeleton.Rmd") %in% zipcontent$Name
    })
    exDir <- file.path(system.file(package = pkg),"rmarkdown/templates")
    unzip(zipfile, exdir = exDir)
    cat(paste("Template",
              template_folder,
              "has been successfully added in\n",
              file.path(exDir, template_folder),
              "\nPlease, restart R session to update template list!"))
  },

  error = function(e)
    stop("No template added!\nThe file provided is not a template zip file!")
  )
}


#' Utility to Remove an Existing RMarkdown Template
#'
#' Remove an Existing Rnssp Rmarkdown template directory.
#'
#' @param template a character string with the name of the template to delete
#' @param pkg a character string with the name of a single package. An error occurs if more than one package name is given.
#'
#' @return a character string
#' @export
#'
#' @examples
#' \dontrun{
#' remove_rmd_template("text_mining") # Remove the Existing Rnssp 'text_mining' template
#' }
remove_rmd_template <- function(template, pkg = "Rnssp", recursive = TRUE, force = TRUE) {
  if(!dir.exists(system.file(package = pkg))){
    stop(paste0("The package '", pkg, "' is not installed!\n"))
  }
  if(!dir.exists(file.path(system.file(package = pkg),"rmarkdown/templates", template))){
    stop(paste0("The template '", template, "' does not exist for the package ", pkg, "!\n"))
  } else{
    unlink(file.path(system.file(package = pkg),"rmarkdown/templates", template), recursive = recursive, force = force)
    if(!dir.exists(file.path(system.file(package = pkg),"rmarkdown/templates", template))){
      cat(paste0("Template ",
                 template,
                 " has been successfully removed from package",
                 pkg,
                 ".\nPlease, restart R session to update template list!"))
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
      regmatches(., regexpr('endDate=\\d+[A-Za-z]+\\d+|end_date=\\d+[A-Za-z]+\\d+', .)) %>%
      str_split("=") %>%
      unlist() %>%
      .[1] %>%
      paste0(., "="),
    spref = url %>%
      regmatches(., regexpr('startDate=\\d+[A-Za-z]+\\d+|start_date=\\d+[A-Za-z]+\\d+', .)) %>%
      str_split("=") %>%
      unlist() %>%
      .[1] %>%
      paste0(., "=")
  )
  old_end <- url %>%
    regmatches(., regexpr('endDate=\\d+[A-Za-z]+\\d+|end_date=\\d+[A-Za-z]+\\d+', .)) %>%
    str_replace(., prefixes[["epref"]], "")
  old_start <- url %>%
    regmatches(., regexpr('startDate=\\d+[A-Za-z]+\\d+|start_date=\\d+[A-Za-z]+\\d+', .)) %>%
    str_replace(., prefixes[["spref"]], "")
  new_end <- old_end
  new_start <- old_start
  if(!is.null(end_date)){
    new_end <- end_date %>%
      as.Date() %>%
      format(., "%e%b%Y") %>%
      str_trim()
  }
  if(!is.null(start_date)){
    new_start <- start_date %>%
      as.Date() %>%
      format(., "%e%b%Y") %>%
      str_trim()
  }
  new_startd <- ifelse(nchar(new_start) > 7,
                       as.Date(new_start, "%e%b%Y"),
                       as.Date(new_start, "%e%b%y"))
  new_endd <- ifelse(nchar(new_end) > 7,
                     as.Date(new_end, "%e%b%Y"),
                     as.Date(new_end, "%e%b%y"))
  if(new_startd > new_endd){
    stop(paste0("Start Date '", new_start, "' is posterior to End Date '", new_end, "'."))
  }
  str_replace(url, old_end, new_end) %>%
    str_replace(., old_start, new_start) %>%
    gsub("[[:space:]]", "", .)
}
