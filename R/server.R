# Mimosa, the mixed models special agent, is a shiny app for 2-level mixed
# models.
#
# Copyright (C) 2019 Johannes Titz
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU Affero General Public License as published by the Free
# Software Foundation, either version 3 of the License, or any later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
# details.
#
# You should have received a copy of the GNU Affero General Public License along
# with this program. If not, see <https://www.gnu.org/licenses/>.


colnames_to_tags <- function(df){
  lapply(
    colnames(df),
    function(co) {
      tag(
        "p",
        list(
          class = class(df[, co]),
          tags$span(class = "glyphicon glyphicon-move"),
          tags$strong(co)
        )
      )
    }
  )
}

#' @importFrom shinyjs show hide
#' @importFrom shinyalert shinyalert
#' @noRd
myserver <- shinyServer(function(input, output, session) {
  # create reactive variables
  reactive <- reactiveValues(data_lambda = data.frame(),
                             level2 = data.frame(),
                             data = data.frame(), r_mdl_formula = "",
                             group_id_selected = character(0),
                             group_ids = character(0),
                             table = NULL)

  data_lambda = reactive({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
      reactive$data_lambda = DF
      DF
    }
  })
  # example data set for tutorial in paper -------------------------------------
  # observe({
  #       query <- parseQueryString(session$clientData$url_search)
  #       if (!is.null(query[['example']])) {
  #         if (query[['example']] == "school") {
  #           data <- mlmRev::Exam
  #           reactive$data <- data
  #           shinyjs::show("create_model")
  #           shinyjs::show("reactive_mode_area")
  #           shinyjs::hide("display_model")
  #           shinyjs::hide("output_region")
  #           shinyjs::hide("help")
  #
  #           id <- find_id(data)
  #           reactive$group_id_selected <- id[1]
  #           reactive$group_ids <- id
  #           result <- determine_levels(id[1], data, show_prog = T)
  #           reactive$level1 <- filter_ivs(result$level1, data)
  #           reactive$level2 <- filter_ivs(result$level2, data)
  #         }
  #       }
  #   })
  output$file_area <- renderUI({
    if(!is.null(input$isSafari)){
      if (as.character(input$isSafari) == "TRUE") {
        accepted_filetype <- "*"
      } else {
         accepted_filetype <- c("text/csv", "text/comma-separated-values",
                                "application/x-spss-sav", "application/x-spss-por",
                                "application/spss", ".sav", ".csv")
       }
      fileInput("datafile", label = NULL, accept = accepted_filetype)
    }
  })
  # read in data file, determine ID and level of variables----------------------
  observeEvent(input$datafile, {
    withProgress(message = "Loading data", value = 0, {
    req(input$datafile)
    data <- load_data(input$datafile)
    reactive$data <- data
    reactive$names <- names(data)
    shinyjs::show("create_model")
    reactive$group_id_selected <- NA
  #           shinyjs::show("reactive_mode_area")
  #           shinyjs::hide("display_model")
  #           shinyjs::hide("output_region")
    shinyjs::hide("help")
    })
  })
  # USER INTERFACE --------------------
  # this is there because data changes, so variables for first panel are not
  # fixed; the other two panels do not have to be here, but this makes it
  # easier to construct the fluid layout
  output$variables <- renderUI({
    fluidRow(
      # PANEL VARIABLES --------------
      column(
        width = 3,
        tags$div(
          class = "panel panel-default",
          tags$div(class = "panel-heading", "Variables"),
          tags$div(
            class = "panel-body",
            id = "sort1",
            colnames_to_tags(reactive$data)
          )
        )
      ),

      column(
        width = 3,
        # PANEL DEPENDENT VARIABLES --------------
        tags$div(
          class = "panel panel-default",
          tags$div(
            class = "panel-heading",
            tags$span(class = "glyphicon glyphicon-stats"),
            "Dependent Variable (drag here)"
          ),
          tags$div(class = "panel-body",
                   id = "sort2")
        ),
        # PANEL BETWEEN --------------
        tags$div(
          class = "panel panel-default",
          tags$div(
            class = "panel-heading",
            tags$span(class = "glyphicon glyphicon-stats"),
            "Independent Variable, between (drag here)"
          ),
          tags$div(class = "panel-body",
                   id = "sort3")
        )
      ),

      column(3, rHandsontableOutput("hot", width = 300)),
      sortable_js(
        "sort1",
        options = sortable_options(
          group = list(name = "sortGroup1",
                       put = TRUE),
          sort = FALSE,
          onSort = sortable_js_capture_input("sort_vars")
        )
      ),
      sortable_js(
        "sort2",
        options = sortable_options(
          group = list(
            group = "sortGroup1",
            put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
            pull = TRUE
          ),
          onSort = sortable_js_capture_input("sort_dv")
        )
      ),
      sortable_js(
        "sort3",
        options = sortable_options(
          group = list(
            group = "sortGroup1",
            put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
            pull = TRUE
          ),
          onSort = sortable_js_capture_input("sort_y")
        )
      )
    )
  })

  #observeEvent(input$sort_y){
    #updateNumericInput(session, "")
  #}

  x <- reactive({
    x <- input$sort_dv
    if (is.character(x)) x %>% trimws()
  })

  y <- reactive({
    input$sort_y %>% trimws()
  })

  # lambda labels
  output$hot <- renderRHandsontable({
    between <- unique(reactive$data[, c(y())])
    #names(dat) <- c("y")
    #print(dat)
    #dat$lambda <- numeric(nrow(dat))
    DF <- data.frame(between, lambda = numeric(length(between)))
    #names(dat) <- c("Between condition", "lambda")
    #DF <- dat
    if (!is.null(DF))
      rhandsontable(DF, stretchH = "all")
  })

  # create table ---------------------------------------------------------------
  output$table_region <- renderPrint({
    validate(
        need(x(), "Drag a variable to Dependent Variable"),
        need(y(), "Drag a variable to Independent Variable, between")
      )

   dat <- reactive$data[, c(x(), y())]
   names(dat) <- c("x", "y")

   dat$y <- as.factor(dat$y)
   data_lambda <- data_lambda()
   print(data_lambda)
   lambda_between <- data_lambda[,2]
   names(lambda_between) <- data_lambda[,1]
   print(lambda_between)
   contr_bw <- calc_contrast(
   dv = x,
   between = y,
   lambda_between = lambda_between,
   data = dat)
   contr_bw
  })
})
