#----------------------------------------------------
# Algorithm Rule Evaluation App
#
# Authors:
#   - Howard Burkom,
#   - Michael Sheppard
#   - Gbedegnon Roseric Azondekon
#
#----------------------------------------------------

# load libraries
suppressPackageStartupMessages({
  packages <- c(
    "shiny", "shinyjs", "dplyr", "Rnssp", "purrr",
    "data.table", "lubridate", "shinycssloaders",
    "plotly", "shinyWidgets", "sf", "shinythemes"
  )
})

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
lapply(packages, library, character.only = TRUE)

if (length(setdiff("Rnssp", rownames(installed.packages()))) > 0) {
  devtools::install_github("cdcgov/Rnssp", upgrade = "never")
}
lapply("Rnssp", library, character.only = TRUE)

# Help Popup
helpPopup <- function(
    id,
    title,
    content,
    placement=c('right', 'top', 'left', 'bottom'),
    trigger=c('click', 'hover', 'focus', 'manual'),
    icon_name = "question-circle",
    icon_style = "color:red"
) {
  tagList(
    singleton(
      tags$head(
        tags$script("$(function() { $(\"[data-toggle='popover']\").popover(); })")
      )
    ),
    HTML(id),
    tags$a(
      href = "#",
      style="margin-left:10px;",
      `data-toggle` = "popover",
      title = title,
      `data-content` = content,
      `data-animation` = TRUE,
      `data-placement` = match.arg(placement, several.ok=TRUE)[1],
      `data-trigger` = match.arg(trigger, several.ok=TRUE)[1],

      shiny::icon(name = icon_name, class = "shinyhelper-icon", style = icon_style)
    )
  )
}

load_profile <- rstudioapi::showQuestion(
  "Alerting Rule Evaluation App",
  "NSSP-ESSENCE Credentials are required to use this app!",
  "Load a profile File",
  "Supply User Credentials"
)

myProfile <- NULL
prof_file <- NULL

if(load_profile){
  filtres = matrix(c("R images (*.RData,*.rda)", "Binary R files (*.rds)",
                     "*.RData;*.rda", "*.rds"), 2, 2)
  if (interactive() && .Platform$OS.type == "windows") {
    prof_file <- choose.files(filters = filtres)
  } else if (interactive() && .Platform$OS.type == "unix") {
    prof_file <- file.choose()
  } else if (!interactive()) {
    prof_file <- readline("Enter full path to the profile file: ")
  }
  if (!any(endsWith(prof_file, c(".rda", ".rds")))) {
    cli::cli_alert_danger("Failed to load. File provided must be either an {.field .rda} or {.field .rds} file")
  }

  if(all(endsWith(tolower(prof_file), ".rda"))){
    myProfile <- get(load(prof_file))
  } else {
    myProfile <- prof_file %>%
      readRDS() %>%
      try(silent = TRUE)
  }
  if(all(class(myProfile) == "try-error")){
    cli::cli_alert_danger("No or corrupt file loaded!")
    myProfile <- create_profile() %>%
      try(silent = TRUE)
    if(all(class(myProfile) == "try-error")){
      cli::cli_abort("App stopped. No credentials provided!")
    }
  }
} else {
  myProfile <- create_profile() %>%
    try(silent = TRUE)
  if(all(class(myProfile) == "try-error")){
    cli::cli_abort("App stopped. No credentials provided!")
  }
}

ccdd_cats <- "https://essence.syndromicsurveillance.org/nssp_essence/api/datasources/va_hosp/fields/ccddCategory" %>%
  get_api_data(profile = myProfile) %>%
  pluck("values") %>%
  pull("value") %>%
  try(silent = TRUE)

if(any(class(ccdd_cats) == "try-error")){
  cli::cli_abort("App failed to establish connection with ESSENCE server!
                 Check your credentials and try again")
}

detectors <- "https://essence.syndromicsurveillance.org/nssp_essence/api/detectors" %>%
  get_api_data(profile = myProfile) %>%
  pluck("detectors") %>%
  filter(
    !id %in% c("c1", "c2", "c3", "probregv2", "shewhartdow"),
    supportsDaily
  ) %>%
  select(id, label)

detector_choices <- setNames(detectors$id, detectors$label)


county_info <- state_sf %>%
  sf::st_drop_geometry() %>%
  select(STATEFP, STUSPS, NAME) %>%
  lapply(., as.character) %>%
  data.frame(., stringsAsFactors = FALSE) %>%
  left_join(
    county_sf %>%
      sf::st_drop_geometry() %>%
      select(STATEFP, NAME) %>%
      lapply(., as.character) %>%
      data.frame(., stringsAsFactors = FALSE),
    by = "STATEFP"
  ) %>%
  rename(STATE = NAME.x, COUNTY = NAME.y)

url1 <- "https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?endDate=25Jun2022&geography="
url2 <- "&percentParam=noPercent&datasource=va_hosp&startDate=25Jun2021&medicalGroupingSystem=essencesyndromes&userId=3751&aqtTarget=TimeSeries&ccddCategory="
url3 <- "&geographySystem=hospitalregion&detector="
url4 <- "&timeResolution=daily&sigDigits=TRUE"


pRed <- 0.01
pYellow <- 0.05
mRed <- 2
nRed <- 7
minCountRed <- 8
mYel <- 2
nYel <- 3
minCountYel <- 4

StartDate_0 <- Sys.Date() %m-%
  months(12)

EndDate_0 <- Sys.Date()

ui <- tagList(
  useShinyjs(),
  tags$head(
    HTML(
      "<script>
          var socket_timeout_interval
          var n = 0
          $(document).on('shiny:connected', function(event) {
          socket_timeout_interval = setInterval(function(){
          Shiny.onInputChange('count', n++)
          }, 15000)
          });
          $(document).on('shiny:disconnected', function(event) {
          clearInterval(socket_timeout_interval)
          });
          </script>
      "
    )
  ),
  theme = shinytheme("cosmo"),
  navbarPage(
    title = "Alerting Rule Evaluation",
    theme = shinytheme("cosmo"),
    id = "nav",
    tabPanel(
      "Evaluation Controls",
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(
              6,
              selectizeInput("State2", "State2", NULL, NULL)
            ),
            column(
              6,
              selectInput("County", "County", NULL, NULL)
            )
          ),
          selectInput("CCDD", "CCDD", ccdd_cats, ccdd_cats[which(grepl("COVID-Specific",ccdd_cats))]),
          selectInput("Detector", "Detector", detector_choices, "probrepswitch"),
          fluidRow(
            column(
              6,
              dateInput(inputId = "StartDate", label = "Start Date", value = StartDate_0)
            ),
            column(
              6,
              dateInput(inputId = "EndDate", label = "End Date", value = EndDate_0)
            )
          ),
          fluidRow(
            column(
              6,
              sliderInput(
                "ReqNumberOfAlerts_R",
                helpPopup(
                  id = "mRed", title = "Minimum of red alerts",
                  content = "For your alerting rule on a given day, the minimum number of red alerts to mark a criterion alert for that day.", "top", "focus"
                ),
                min = 0, max = 7, value = 2
              )
            ),
            column(
              6,
              sliderInput(
                "AlertingInterval_R",
                helpPopup(
                  id = "nRed", title = "Time window in days",
                  content = "Including today, for counting ‘mRed’ red alerts to activate your rule and alert you (mRed = 3 and nRed = 4 means 3 red alerts within the last 4 days)",
                  "top", "focus"
                ),
                min = 0, max = 7, value = 2
              )
            )
          ),
          fluidRow(
            column(
              6,
              numericInput(
                inputId = "MinCaseCount_R",
                helpPopup(
                  id = "MinCountRed", title = "Total number of records",
                  content = "Optional. Total number of records required for your alerting rule (mRed = 3 and nRed = 4 and minCountRed = 6 means 3 red alerts within the last 4 days if there are at least 6 records in the 4 days) – leave blank to skip this option",
                  "top", "focus"
                ),
                value = minCountRed
              )
            ),
            column(
              6,
              numericInput(
                inputId = "Pval_R",
                helpPopup(
                  id = "Pvalue_Red", title = "Maximum p-value",
                  content = "p-value threshold required for a red alert for your alerting rule (widely used default = 0.01)",
                  "top", "focus"
                ),
                value = pRed
              )
            )
          ),
          fluidRow(
            column(
              6,
              sliderInput(
                "ReqNumberOfAlerts_Y",
                helpPopup(
                  id = "mYellow", title = "Minimum of yellow alerts",
                  content = "For your alerting rule on a given day, the minimum number of yellow alerts to mark a criterion alert for that day.",
                  "top", "focus"
                ),
                min = 0, max = 7, value = 2
              )
            ),
            column(
              6,
              sliderInput(
                "AlertingInterval_Y",
                helpPopup(
                  id = "nYellow", title = "Time window in days",
                  content = "Including today, for counting ‘mYellow’ yellow alerts to activate your rule and alert you (mYellow = 3 and nYellow = 4 means 3 yellow alerts within the last 4 days)",
                  "top", "focus"
                ),
                min = 0, max = 7, value = 2
              )
            )
          ),
          fluidRow(
            column(
              6,
              numericInput(
                inputId = "MinCaseCount_Y",
                helpPopup(
                  id = "MinCountYellow", title = "Total number of records",
                  content = "Optional. Total number of records required for your alerting rule (mYellow = 3 and nYellow = 4 and minCountYellow = 6 means 3 yellow alerts within the last 4 days if there are at least 6 records in the 4 days)",
                  "top", "focus"
                ),
                value = minCountYel
              )
            ),
            column(
              6,
              numericInput(
                inputId = "Pval_Y",
                helpPopup(
                  id = "PValueYellow", title = "Maximum p-value",
                  content = "p-value threshold required for a yellow alert for your alerting rule (widely used default = 0.05)",
                  "top", "focus"
                ),
                value = pYellow
              )
            )
          ),
          br(),
          fluidRow(
            column(
              width=4,
              checkboxGroupInput(
                "markers", "Markers to show:",
                choices = c("Red/Yellow" = "RedYel", "Criterion" = "Crit"),
                selected = c("RedYel")
              )
            ),
          ),
          hr(),
          actionButton("go", "Load Data")
        ),
        mainPanel(
          helpPopup(
            id = "", title = "",
            content = paste0(
              "The settings allow testing of an ‘m-out-of-n’ alerting criterion, ",
              "meaning ‘m alerts in the last n days. The settings allow you to set ",
              "an alert if there are (m out of n red alerts) OR (m out of n yellow alerts), ",
              "with an option to require a minimum number of records in the last n days. ",
              "The p-value limits may also be changed from the default values of 0.01 ",
              "for red and 0.05 for yellow alerts."
            ),
            placement = "bottom", trigger = "focus",
            icon_name = "bell",
            icon_style = "color:blue;font-size:15px"
          ),
          br(), br(),
          fluidRow(
            tableOutput("table")
          ),
          fluidRow(
            verbatimTextOutput("summary")
          ),
          fluidRow(
            column(
              width = 12,
              wellPanel(
                "Plot",
                width = "1200px",
                withSpinner(plotlyOutput(outputId = "tsPlotly", height = "550px"))
              ),
              textOutput("text")
            )
          )
        )
      )
    )
  )
)



server <- function(input, output, session) {
  observe({
    updateSelectizeInput(
      session,
      "State2",
      choices = county_info %>%
        pull(STUSPS) %>%
        unique() %>%
        sort(),
      selected = "OR",
      server = TRUE
    )
  })

  observeEvent(input$State2, {
    updateSelectizeInput(
      session,
      "County",
      choices = county_info %>%
        filter(STUSPS == input$State2) %>%
        pull(COUNTY),
      selected = "Multnomah",
      server = TRUE
    )
  })

  # To avoid RStudio timeouts -- server code
  output$keepAlive <- renderText({
    req(input$count)
    paste("keep alive ", input$count)
  })

  output$summary <- renderPrint(
    {
      summary(df1()$count)
    },
    width = 10
  )

  url <- eventReactive(input$go, {
    input$County %>%
      tolower() %>%
      gsub(" ", "%20", .) %>%
      paste0(url1, tolower(input$State2), "_", .,
             url2, gsub(" ", "%20", input$CCDD),
             url3, input$Detector, url4) %>%
      change_dates(input$StartDate, input$EndDate)
  })

  df1 <- reactive({
    api_data <- myProfile$get_api_data(url())
    df <- api_data$timeSeriesData
    df %>%
      mutate(
        date = as.Date(date),
        redCounts = ifelse(levels <= input$Pval_R, count, NA),
        yellowCounts = ifelse(levels <= input$Pval_Y & levels > input$Pval_R, count, NA),
        alertRollSumR = frollsum(x = levels < input$Pval_R, n = input$AlertingInterval_R, fill = NA),
        countRollSumR = frollsum(x = count, n = input$AlertingInterval_R, fill = NA),
        alertRollSumY = frollsum(x = levels < input$Pval_Y, n = input$AlertingInterval_Y, fill = NA),
        countRollSumY = frollsum(x = count, n = input$AlertingInterval_Y, fill = NA),
        criterion = ifelse((((alertRollSumR >= input$ReqNumberOfAlerts_R) & (countRollSumR >= input$MinCaseCount_R)) |
                              ((alertRollSumY >= input$ReqNumberOfAlerts_Y) & (countRollSumY >= input$MinCaseCount_Y))) &
                             (!is.na(redCounts) | !is.na(yellowCounts)), count, NA),
        levels = as.numeric(levels),
        blueCounts = ifelse(is.na(redCounts) & is.na(yellowCounts), count, NA)
      )
  })

  output$table <- renderTable({
    nrCriteria <- sum(df1()$criterion > 0, na.rm = TRUE)
    nrRed <- sum(!is.na(df1()$redCounts))
    nrYellow <- sum(!is.na(df1()$yellowCounts))
    nrZeros <- sum(df1()$count == 0)
    tsLength <- nrow(df1())
    tableNames <- c(
      "Observations", "Red Alerts",
      "Yellow Alerts", "Zero Daily Counts", "Criterion Alerts"
    )
    tableValues <- c(tsLength, nrRed, nrYellow, nrZeros, nrCriteria)
    df_table <- as.data.table(matrix(tableValues, nrow = 1))
    names(df_table) <- tableNames
    df_table
  })

  oPlot <- reactive({
    input$go
    df1()

    plt <- plot_ly(data = df1()) %>%
      add_trace(
        x = ~date,
        y = ~count,
        line = list(color = "rgb(22, 96, 167)", width = 0.5),
        type = "scatter",
        mode = "lines",
        showlegend = FALSE,
        hoverinfo = "text",
        text = ~paste(
          "<br>Date:</b>", date,
          "<br>Count:</b>", format(count, big.mark = ","),
          "<br>p-value:</b>", format(levels, digits = 2, scientific = TRUE)
        )
      ) %>%
      add_markers(
        x = ~date,
        y = ~blueCounts,
        marker = list(color = "rgb(22, 96, 167)", line = list(color = "black", width = 0.5)),
        hoverinfo = "text",
        text = ~paste(
          "<br>Date:</b>", date,
          "<br>Count:</b>", format(count, big.mark = ","),
          "<br>p-value:</b>", format(levels, digits = 2, scientific = TRUE)
        ),
        name = "None"
      ) %>%
      layout(
        xaxis = list(
          title = "Date",
          showspikes = TRUE,
          spikemode = "across",
          ticks = "outside",
          spikedash = "dot",
          spikecolor = "black",
          spikethickness = -2
        ),
        yaxis = list(
          title = "ED Encounters",
          showline = TRUE,
          rangemode = "tozero",
          ticks = "outside"
        )
      )

    if ("RedYel" %in% input$markers) {
      plt <- plt %>%
        add_markers(
          x = ~date,
          y = ~yellowCounts,
          marker = list(color = "#FFC107", line = list(color = "black", width = 0.5)),
          hoverinfo = "text",
          text = ~paste(
            "<br>Date:</b>", date,
            "<br>Count:</b>", format(count, big.mark = ","),
            "<br>p-value:</b>", format(levels, digits = 2, scientific = TRUE)
          ),
          name = "Warning"
        ) %>%
        add_markers(
          x = ~date,
          y = ~redCounts,
          marker = list(color = "#DC3545", line = list(color = "black", width = 0.5)),
          hoverinfo = "text",
          text = ~paste(
            "<br>Date:</b>", date,
            "<br>Count:</b>", format(count, big.mark = ","),
            "<br>p-value:</b>", format(levels, digits = 2, scientific = TRUE)
          ),
          name = "Alert"
        )

    }

    if ("Crit" %in% input$markers) {
      plt <- plt %>%
        add_markers(
          x = ~date,
          y = ~criterion,
          mode = "markers",
          marker = list(symbol = "diamond-open", size = 10, color = "black"),
          hoverinfo = "text",
          text = ~paste(
            "<br>Date:</b>", date,
            "<br>Count:</b>", format(count, big.mark = ","),
            "<br>p-value:</b>", format(levels, digits = 2, scientific = TRUE)
          ),
          name = "Criterion Met"
        )

    }

    plt %>%
      config(modeBarButtons = list(list("toImage"), list("autoScale2d")))


  })

  output$tsPlotly <- renderPlotly({
    oPlot()
  })
}

shinyApp(ui, server)
