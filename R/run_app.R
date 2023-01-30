#' Run embedded shiny applications
#'
#' @param app A string representing the name of the application to run
#' @param launch_browser logical. Should the app be launch in the system's
#'     default browser? (default is TRUE)
#'
#' @export
#'
#' @examples
#' run_app("burkom")
run_app <- function(app, launch_browser = TRUE) {
  available_apps <- list.files(system.file("shiny-apps", package = "Rnssp"))

  available_apps_msg <-  available_apps %>%
    paste0("{.var ", ., "}", collapse = "', ") %>%
    paste0("Available apps are: ", .)

  if (missing(app) || !nzchar(app) || !app %in% available_apps) {
    available_apps_msg %>%
      paste0("Please execute {.fn run_app} with a valid app name as an argument.\n", .) %>%
      cli::cli_abort()
  }

  app_dir <- system.file("shiny-apps", app, package = "Rnssp")
  shiny::runApp(app_dir, display.mode = "normal", launch.browser = launch_browser) %>%
    suppressWarnings()
}
