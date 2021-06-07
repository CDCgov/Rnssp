.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the
 ____
|  _ \\ _ __  ___ ___ _ __
| |_) | '_ \\/ __/ __| '_ \\
|  _ <| | | \\__ \\__ \\ |_) |
|_| \\_\\_| |_|___/___/ .__/   package version 0.0.1,
                    |_|
A Signature R package for the National Syndromic and Surveillance Program at
the Centers for Disease Control and Prevention (CDC)

Full Documentation available at: https://cdcgov.github.io/Rnssp
Run 'Rnssp_vgnettes()' to browse all Rnssp vignettes.
")
}

# .onLoad <- function(libname, pkgname) {
#   op <- options()
#   op.Rnssp <- list(
#     Rnssp.path = "~/R-dev",
#     Rnssp.install.args = "",
#     Rnssp.name = "Gbedegnon Roseric Azondekon",
#     Rnssp.desc.author = "Gbedegnon Roseric Azondekon <gazondekon@cdc.gov> [aut, cre]",
#     Rnssp.desc.license = "MIT",
#     Rnssp.desc.suggests = NULL,
#     Rnssp.desc = list()
#   )
#   toset <- !(names(op.Rnssp) %in% names(op))
#   if(any(toset)) options(op.Rnssp[toset])
#
#   invisible()
# }
