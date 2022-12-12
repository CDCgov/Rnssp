#' Run embedded shiny applications
#'
#' @param app A string representing the name of the application to run
#'
#' @return
#' @export
#'
#' @examples
run_app <- function(app) {
  available_apps <- list.files(system.file("shiny-apps", package = "Rnssp"))

  available_apps_msg <-  available_apps %>%
    paste0("{.var ", ., "}", collapse = "', ") %>%
    paste0("Available apps are: ", .)

  if (missing(app) || !nzchar(app) || !app %in% available_apps) {
    available_apps_msg %>%
      paste0("Please execute {.fn run_app} with a valid app name as an argument.\n", .) %>%
      cli::cli_alert_info()
    return(NULL)
  }

  app_dir <- system.file("shiny-apps", app, package = "mypackage")
  shiny::runApp(app_dir, display.mode = "normal")
}
