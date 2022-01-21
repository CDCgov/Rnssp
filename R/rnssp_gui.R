#' Add Rnssp template
#'
#' Wrapper around \code{\link[=add_rmd_template]{add_rmd_template()}} to add Rnssp templates from
#' the Rnssp templates Github repository.
#'
#' @keywords internal
#'
add_rmd_template_gui <- function() {
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(
      "Add/Update Rnssp RMD Templates",
      right = miniUI::miniTitleBarButton("done", "Add/Update", primary = TRUE)
    ),
    miniUI::miniContentPanel(
      shiny::column(
        12,
        DT::dataTableOutput("table"),
        shiny::tags$script(
          shiny::HTML(
            '$(document).on("click", "input", function () {
              var checkboxes = document.getElementsByName("selected");
              var checkboxesChecked = [];
              for (var i=0; i<checkboxes.length; i++) {
                if (checkboxes[i].checked) {
                  checkboxesChecked.push(checkboxes[i].value);
                }
              }
              Shiny.onInputChange("checked_rows",checkboxesChecked);  })'
          )
        )
      )
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

    datatable2 <- function(x, vars = NULL, opts = NULL, ...) {
      names_x <- names(x)
      if (is.null(vars)) stop("'vars' must be specified!")
      pos <- match(vars, names_x)
      if (any(purrr::map_chr(x[, pos], typeof) == "list")) {
        stop("list columns are not supported in datatable2()")
      }

      pos <- pos[pos <= ncol(x)] + 1
      rownames(x) <- NULL
      if (nrow(x) > 0) x <- cbind(" " = "&oplus;", x)

      # options
      opts <- c(
        opts,
        list(
          columnDefs = list(
            list(visible = FALSE, targets = c(0, pos)),
            list(orderable = FALSE, className = "details-control", targets = 1),
            list(className = "dt-left", targets = 1:3),
            list(className = "dt-right", targets = 4:ncol(x))
          )
        )
      )

      DT::datatable(
        x,
        ...,
        escape = FALSE,
        options = opts,
        callback = DT::JS(.callback2(x = x, pos = c(0, pos)))
      )
    }

    .callback2 <- function(x, pos = NULL) {
      part1 <- "table.column(1).nodes().to$().css({cursor: 'pointer'});"

      part2 <- .child_row_table2(x, pos = pos)

      part3 <- "
        table.on('click', 'td.details-control', function() {
          var td = $(this), row = table.row(td.closest('tr'));
          if (row.child.isShown()) {
            row.child.hide();
            td.html('&oplus;');
          } else {
          row.child(format(row.data())).show();
          td.html('&ominus;');
        }
      });"

      paste(part1, part2, part3)
    }

    .child_row_table2 <- function(x, pos = NULL) {
      names_x <- paste0(names(x), ":")
      text <- "
        var format = function(d) {
          text = '<div><table >' +
      "

      for (i in seq_along(pos)) {
        text <- paste(text, glue::glue(
          "'<tr>' +
          '<td>' + '{names_x[pos[i]]}' + '</td>' +
          '<td>' + d[{pos[i]}] + '</td>' +
        '</tr>' + "
        ))
      }

      paste0(
        text,
        "'</table></div>'
      return text;};"
      )
    }

    output$table <- DT::renderDataTable({
      datatable2(
        x = template_df,
        vars = c("name", "description", "documentation"),
        opts = list(pageLength = 10, searching = FALSE, lengthChange = FALSE, scrollY = "400px")
      )
    })

    shiny::observeEvent(input$done, {
      if (is.null(input$checked_rows)) {
        shiny::stopApp()
      }
      for (templ in input$checked_rows) {
        Rnssp::add_rmd_template(templ)
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
