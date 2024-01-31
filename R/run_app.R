#' Run embedded shiny applications
#'
#' @param app A string representing the name of the application to run
#' @param launch_browser logical. Should the app be launch in the system's
#'     default browser? (default is TRUE)
#'
#' @export
#'
#' @examples
#' run_app("rule_eval")
run_app <- function(app, launch_browser = TRUE) {
  apps_dir <- system.file("shiny-apps", package = "Rnssp")
  local_apps <- list.files(apps_dir)
  remote_apps <- Rnssp::list_apps()
  available_apps <- unique(c(local_apps, remote_apps))
  
  available_apps_msg <- paste0(
    "Available apps are: ",
    paste0("{.var ", available_apps, "}", collapse = "', ")
  )
  
  if(missing(app) || !nzchar(app) || !app %in% available_apps) {
    cli::cli_abort(
      paste0(
        "Please execute {.fn run_app} with a valid app name as an argument.\n", 
        available_apps_msg
      )
    )
  }
  
  if(app %in% local_apps){
    app_dir <- system.file("shiny-apps", app, package = "Rnssp")
    suppressWarnings(
      shiny::runApp(
        app_dir, 
        display.mode = "normal", 
        launch.browser = launch_browser
      )
    )
  } else {
    shiny::runGitHub(
      repo = "Rnssp-shiny-apps", 
      username = "cdcgov",
      subdir = app, 
      destdir = apps_dir, 
      ref = "master", 
      launch.browser = launch_browser
    )
  }
}
