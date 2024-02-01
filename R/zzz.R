.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the
 ____
|  _ \\ _ __  ___ ___ _ __
| |_) | '_ \\/ __/ __| '_ \\
|  _ <| | | \\__ \\__ \\ |_) |
|_| \\_\\_| |_|___/___/ .__/   package version 0.3.0 beta,
                    |_|
A Signature R package for the National Syndromic Surveillance Program at
the Centers for Disease Control and Prevention (CDC)

Full Documentation available at: https://cdcgov.github.io/Rnssp
Rnssp RMD Templates Documentation: https://cdcgov.github.io/Rnssp-rmd-templates

Run 'Rnssp_vignettes()' to browse all Rnssp vignettes.
")
  url <- paste0("https://raw.githubusercontent.com/cdcgov/Rnssp/master/DESCRIPTION")

  x <- suppressWarnings(try(readLines(url), silent = TRUE))
  remote_version <- gsub("Version:\\s*", "", x[grep('Version:', x)])
  installed_version <- tryCatch(packageVersion(gsub(".*/", "", "Rnssp")), error=function(e) NA)

  if (all(remote_version > installed_version, class(x) != "try-error")) {
    cli::cli_alert_info(
      paste0(
        'Rnssp v',
        installed_version,
        ' is outdated!\n## Please upgrade to latest Rnssp v',
        remote_version,
        ': {.code devtools::install_github("cdcgov/Rnssp")}')
    )
  }
}

