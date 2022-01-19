#' Remove Rnssp templates
#'
#' Wrapper around \code{\link[=remove_rmd_template]{remove_rmd_template()}}
#' to Remove Existing NSSP RMarkdown Templates.
#'
#' @keywords internal
#'
remove_rmd_template_gui <- function() {
  templates <- basename(
    list.dirs(
      file.path(
        system.file(package = "Rnssp"), "rmarkdown/templates"), recursive = FALSE)
  )

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(
      "Remove Rnssp RMD Templates",
      right = miniUI::miniTitleBarButton("done", "Remove", primary = TRUE)
    ),
    miniUI::miniContentPanel(
      shiny::checkboxGroupInput("templ",
                                label = "Existing templates",
                                inline = TRUE,
                                choices = templates)
    )
  )

  server <- function(input, output, session) {

    shiny::observeEvent(input$done, {
      if(is.null(input$templ)){
        shiny::stopApp()
      }
      for(templ in input$templ){
        Rnssp::remove_rmd_template(templ)
      }
      shiny::stopApp()
    })

    shiny::observeEvent(input$cancel, {
      shiny::stopApp()
    })

  }

  viewer <- shiny::dialogViewer("Add")
  shiny::runGadget(ui, server, viewer = viewer)

}
