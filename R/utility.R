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
