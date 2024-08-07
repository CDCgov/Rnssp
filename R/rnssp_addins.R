#' Add Rnssp template
#'
#' Wrapper around \code{\link[=add_rmd_template]{add_rmd_template()}} to add Rnssp templates from
#' the Rnssp templates Github repository.
#'
#' @keywords internal
#'
add_rmd_template_gui <- function() {
  
  #set working directory and add resource path to load images from
  setwd(system.file("www", package = "Rnssp"))
  shiny::addResourcePath(prefix = "appResources", directoryPath = ".")
  
  ui <- miniUI::miniPage(
    #load css file
    shiny::tags$head(shiny::includeCSS("app_picker_gui_styles.css")),
    
    miniUI::gadgetTitleBar(
      "Add/Update Rnssp RMD Templates",
      right = miniUI::miniTitleBarButton("done", "Add/Update", primary = TRUE)
    ),
    miniUI::miniContentPanel(
      shiny::uiOutput("cards_ui")
    )
  )

  server <- function(input, output, session) {
    
    template_df <- dplyr::mutate(
      dplyr::rename(
        dplyr::select(
          Rnssp::list_templates(TRUE), -create_dir
        ),
        template = id
      ),
      select = paste0('<input type="checkbox" name="selected" value="', template, '">'),
      documentation = paste0(
        "<a href='",
        file.path(
          "https://cdcgov.github.io/Rnssp-rmd-templates/templates",
          stringr::str_remove_all(template, "_")
        ),
        "/' target='_blank'>Full documentation</a>"
      )
    )
    
    #function to generate a given card UI
    get_card <- function(id = "id", title = "Title", text = "text", authors = "Authors"){
      image_div <- shiny::tags$img(src = paste0("https://github.com/CDCgov/Rnssp-shiny-apps/blob/master/", id, "/thumbnail.jpg?raw=true"), onerror=paste0("this.onerror=null; this.src='", file.path("appResources", "default_thumbnail.png"), "'"), width = "250px", height = "100px")
      text_div <- shiny::tags$div(style = "font-size:14px;line-height:105%;max-height:200px;overflow-y:scroll;padding-top:1rem;padding-bottom:1rem;",
                                  shiny::tags$p(style = "padding:.5rem;", text))
      content <- shiny::tags$div(style = "display: flex;flex-direction: column;",
                                 image_div,
                                 shiny::tags$h3(class = "card-title", title),
                                 #shiny::tags$h6(class = "card-authors", authors),
                                 text_div)
      shiny::tags$li(class = "card",
                     id = id,
                     onclick = "this.classList.toggle('selected');
                            var card_ele = document.getElementsByClassName('card');
                            var selected_template_ids = [];
                            for (var i = 0; i < card_ele.length; ++i){
                              if(card_ele[i].classList.contains('selected')){
                                selected_template_ids.push(card_ele[i].id);
                              }
                            };
                            Shiny.setInputValue('selected_templates', {selected_templates: selected_template_ids, time: Date()})",
                     shiny::tags$div(class = "Card-content", content)
                     
      )
    }
    #function to generate card container UI
    get_cards <- function(cards_list = shiny::tagList(get_card(), get_card())){
      shiny::tags$div(class = "container",
                      shiny::tags$div(class = "grid", 
                                      shiny::tags$ul(class = "cards",
                                                     cards_list
                                      )
                      )
      )
    }

    #cards UI
    output$cards_ui <- shiny::renderUI({
      get_cards(
        shiny::tagList(lapply(1:nrow(template_df), function(i) get_card(id = template_df$template[i],
                                                                   title = template_df$name[i],
                                                                   text = template_df$description[i],
                                                                   #authors = template_df$author[i]
                                                                   ))
        )
      )
    })

    shiny::observeEvent(input$done, {
      
      selected_templates <- sapply(input$selected_templates$selected_templates, function(x) x)
      
      if (is.null(selected_templates)) {
        shiny::stopApp()
      }
      for (templ in selected_templates) {
        Rnssp::add_rmd_template(templ, restart = FALSE, verbose = FALSE)
      }
      rstudioapi::restartSession()
      shiny::stopApp()
    })

    shiny::observeEvent(input$cancel, {
      shiny::stopApp()
    })
  }

  viewer <- shiny::dialogViewer("Add", width = 1250, height = 1000)
  shiny::runGadget(ui, server, viewer = viewer)
}


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
        system.file(package = "Rnssp"), "rmarkdown/templates"
      ),
      recursive = FALSE
    )
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
                                choices = templates
      )
    )
  )

  server <- function(input, output, session) {
    shiny::observeEvent(input$done, {
      if (is.null(input$templ)) {
        shiny::stopApp()
      }
      for (templ in input$templ) {
        Rnssp::remove_rmd_template(templ, restart = FALSE, verbose = FALSE)
      }
      rstudioapi::restartSession()
      shiny::stopApp()
    })

    shiny::observeEvent(input$cancel, {
      shiny::stopApp()
    })
  }

  viewer <- shiny::dialogViewer("Add")
  shiny::runGadget(ui, server, viewer = viewer)
}


#' User profile skeleton
#'
#' Generate a user profile skeleton script into the console.
#'
#' @keywords internal
#'
create_user_profile <- function() {
  skeleton <- 'library("Rnssp")
myProfile <- create_profile()'
  if (any((.packages()) == "Rnssp")) {
    skeleton <- 'myProfile <- create_profile()'
  }
  rstudioapi::sendToConsole(skeleton, execute = FALSE)
}

#' Create User Profile (GUI)
#'
#' Create and/or save a user profile
#'
#' @keywords internal
#'
create_user_profile_gui <- function() {
  alert_msg <- function(x, y) {
    if (class(x) == "try-error") {
      cli::cli_alert_danger("Failed to save {.file {y}}")
      shiny::stopApp()
    } else {
      cli::cli_alert_success(paste("User Profile saved to", "{.file {y}}"))
    }
  }

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(
      "Create/Save User Credentials",
      right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE)
    ),
    miniUI::miniContentPanel(
      shiny::textInput("username", "Username", placeholder = "Enter your username!"),
      shiny::passwordInput("password", "Password", placeholder = "Enter your password!"),
      shiny::tags$details(
        shiny::tags$summary("Change user profile variable name"),
        shiny::textInput("filename", "Filename", "myProfile")
      ),
      shiny::column(
        12,
        shiny::checkboxInput("saveProfile", label = "Save Profile to Home Directory?"),
        shiny::conditionalPanel(
          condition = "input.saveProfile == true",
          shiny::radioButtons("format", "Select a format", inline = TRUE, choices = c(".rda", ".rds"), selected = ".rda"),
        )
      )
    )
  )

  server <- function(input, output, session) {
    shiny::observeEvent(input$done, {
      filename <- input$filename
      if (any(length(input$username) == 0, length(input$password) == 0)) {
        shiny::stopApp()
      }
      if (grepl("[[:punct:][:space:]]", filename)) {
        cli::cli_abort("Variable name {.var {filename}} is invalid! Try again!")
      } else {
        myProfile <- Rnssp::create_profile(input$username, input$password)
        assign(
          filename,
          value = myProfile,
          envir = .GlobalEnv
        )
      }

      if (input$saveProfile) {
        target <- file.path(Sys.getenv("HOME"), paste0(filename, input$format))
        if (input$format == ".rda") {
          saveFile <- try(save(myProfile, file = target), silent = TRUE)
          alert_msg(saveFile, target)
        } else {
          saveFile <- try(saveRDS(myProfile, file = target), silent = TRUE)
          alert_msg(saveFile, target)
        }
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

#' Run Rnssp Shinyapps
#'
#' Wrapper around \code{\link[=run_app]{run_app()}} to run Rnssp Shinyapps from
#' the Rnssp Shinyapps Github repository.
#'
#' @keywords internal
#'
run_app_gui <- function() {
  #set working directory and add resource path to load images from
  setwd(system.file("www", package = "Rnssp"))
  shiny::addResourcePath(prefix = "appResources", directoryPath = ".")

  ui <- miniUI::miniPage(
    #load css file
    shiny::tags$head(shiny::includeCSS("app_picker_gui_styles.css")),
    #gadget UI
    miniUI::gadgetTitleBar(
      "Run Rnssp Shinyapps",
      right = miniUI::miniTitleBarButton("done", "Run/Execute", primary = TRUE)
    ),
    miniUI::miniContentPanel(
      shiny::uiOutput("cards_ui")
    )
  )

  server <- function(input, output, session) {
    #pull app data
    app_df <- dplyr::mutate(
      dplyr::rename(
        Rnssp::list_apps(TRUE),
        app = id
      ),
      select = paste0('<input type="radio" name="selected" value="', app, '">')#,
    )

    #function to generate a given card UI
    get_card <- function(id = "id", title = "Title", text = "text", authors = "Authors"){
      image_div <- shiny::tags$img(src = paste0("https://github.com/CDCgov/Rnssp-shiny-apps/blob/master/", id, "/thumbnail.jpg?raw=true"), onerror=paste0("this.onerror=null; this.src='", file.path("appResources", "default_thumbnail.png"), "'"), width = "250px", height = "100px")
      text_div <- shiny::tags$div(style = "font-size:14px;line-height:105%;max-height:200px;overflow-y:scroll;padding-top:1rem;padding-bottom:1rem;",
                                  shiny::tags$p(style = "padding:.5rem;", text))
      content <- shiny::tags$div(style = "display: flex;flex-direction: column;",
                                 image_div,
                                 shiny::tags$h3(class = "card-title", title),
                                 shiny::tags$h6(class = "card-authors", authors),
                                 text_div)
      shiny::tags$li(class = "card",
                     id = id,
                     onclick = "var card_ele = document.getElementsByClassName('card');
                            for (var i = 0; i < card_ele.length; ++i) card_ele[i].classList.remove('selected');
                            this.classList.toggle('selected');
                            Shiny.setInputValue('selected_app', {app_name: this.id, time: Date(), is_selected: this.classList.contains('selected')})",
                     shiny::tags$div(
                       #shiny::tags$h3(class = "card-title", title),
                       shiny::tags$div(class = "Card-content", content)
                     )
      )
    }
    #function to generate card container UI
    get_cards <- function(cards_list = shiny::tagList(get_card(), get_card())){
      shiny::tags$div(class = "container",
                      shiny::tags$div(class = "grid", 
                                      shiny::tags$ul(class = "cards",
                                                     cards_list
                                      )
                      )
      )
    }
    #cards UI
    output$cards_ui <- shiny::renderUI({
      get_cards(
        shiny::tagList(lapply(1:nrow(app_df), function(i) get_card(id = app_df$app[i],
                                                                   title = app_df$name[i],
                                                                   text = app_df$description[i],
                                                                   authors = app_df$author[i]))
        )
      )
    })
    #execute button functionality
    shiny::observeEvent(input$done, {
      app_name <- input$selected_app$app_name
      if (!is.null(app_name)) {
        rstudioapi::sendToConsole(
          paste0("Rnssp::run_app('", app_name, "')"),
          execute = TRUE
        )
      }
      shiny::stopApp()
    })
    #cancel button functionality
    shiny::observeEvent(input$cancel, {
      shiny::stopApp()
    })
  }
  #runGadget call
  viewer <- shiny::dialogViewer("Add", width = 1250, height = 1000)
  shiny::runGadget(ui, server, viewer = viewer)
}
