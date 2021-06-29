#' @importFrom shinyjs show hide
#' @importFrom shinyalert shinyalert
#' @importFrom rhandsontable renderRHandsontable rHandsontableOutput hot_col
#' @importFrom sortable sortable_js
#' @noRd
myserver <- shinyServer(function(input, output, session) {
  # create reactive variables
  reactive <- reactiveValues()
  # example data sets ----------------------------------------------------------
  observe({
        query <- parseQueryString(session$clientData$url_search)
        print(query)
        if (!is.null(query[["example"]])) {
          file <- paste(query[["example"]], sep = "")
          data(list = file, package = "cofad")
          eval(parse(text = paste("data <- ", query[["example"]], sep = "")))
          reactive$data <- data
          shinyjs::show("create_model")
          shinyjs::hide("output_region")
          shinyjs::hide("help")
        }
    })
  # upload file area -----------------------------------------------------------
  output$file_area <- renderUI({
    if (!is.null(input$isSafari)) {
      if (as.character(input$isSafari) == "TRUE") {
        accepted_filetype <- "*"
      } else {
        accepted_filetype <- c("text/csv", "text/comma-separated-values",
                               "application/x-spss-sav",
                               "application/x-spss-por",
                               "application/spss", ".sav", ".csv")
      }
      fileInput("datafile", label = NULL, accept = accepted_filetype)
    }
  })
  # read in data file-----------------------------------------------------------
  observeEvent(input$datafile, {
    withProgress(message = "Loading data", value = 0, {
    req(input$datafile)
    data <- load_data(input$datafile)
    # if same data, do not do anything
    validate(need(!identical(data, reactive$data), "same data"))

    reactive$data <- data

    # activate some panels
    shinyjs::show("create_model")
    shinyjs::hide("help")
    shinyjs::show("output_region")

    # set reactive values
    reactive$lambda_between <- NULL
    reactive$lambda_within <- NULL
    })
  })
  # USER INTERFACE -------------------------------------------------------------
  # this is here because data changes, so variables for the first panel are not
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
           id = "sort_variables",
           colnames_to_tags(reactive$data)
         )
       )
     ),
     column(
       width = 8,
       fluidRow(
         column(
           # PANEL DEPENDENT VARIABLES --------------
           width = 4,
           tags$div(
             class = "panel panel-default",
             tags$div(
               class = "panel-heading",
               tags$span(class = "glyphicon"),
               "DV"
             ),
             tags$div(class = "panel-body", id = "sort_dv_name")
           )
         ),
         column(
           width = 5,
           tags$div(
             class = "panel panel-default",
             tags$div(
               class = "panel-heading",
               tags$span(class = "glyphicon"),
               "ID Variable"
             ),
             tags$div(class = "panel-body", id = "sort_id_name")
           )
         )
       ),
       fluidRow(
         # PANEL BETWEEN --------------
         column(
           width = 4,
           tags$div(
             class = "panel panel-default",
             tags$div(
               class = "panel-heading",
               tags$span(class = "glyphicon"),
               "IV, between"
             ),
             tags$div(class = "panel-body", id = "sort_between_name")
           )
         ),
         # lambda between table ----
         column(
           width = 5,
           rhandsontable::rHandsontableOutput("hot_lambda_btw", width = 250)
         )
       ),
       fluidRow(
         # PANEL WITHIN --------------
         column(
           width = 4,
           tags$div(
             class = "panel panel-default",
             tags$div(
               class = "panel-heading",
               tags$span(class = "glyphicon"),
               "IV, within"
             ),
             tags$div(class = "panel-body", id = "sort_within_name")
           )
         ),
         # lambda within table ----
         column(
           width = 5,
           rhandsontable::rHandsontableOutput("hot_lambda_wi", width = 200)
         )
       )
     ),
     # main part of UI finished, now add the sortable_js configuration----------
     sortable::sortable_js(
       "sort_variables",
       options = sortable::sortable_options(
         group = list(name = "sortGroup1", put = TRUE),
         sort = FALSE,
         onSort = sortable::sortable_js_capture_input("sort_vars"),
         # onLoad is crucial, otherwise the old value will remain for the
         # capture
         onLoad = sortable::sortable_js_capture_input("sort_vars")
       )
     ),
     sortable::sortable_js(
       "sort_dv_name",
       options = sortable::sortable_options(
         group = list(
           group = "sortGroup1",
           put = htmlwidgets::JS(
             "function (to) { return to.el.children.length < 1; }"
           ),
           pull = TRUE
         ),
         onSort = sortable::sortable_js_capture_input("sort_dv_name"),
         onLoad = sortable::sortable_js_capture_input("sort_dv_name")
       )
     ),
     sortable::sortable_js(
       "sort_between_name",
       options = sortable::sortable_options(
         group = list(
           group = "sortGroup1",
           put = htmlwidgets::JS(
             "function (to) { return to.el.children.length < 1; }"
           ),
           pull = TRUE
         ),
         onSort = sortable::sortable_js_capture_input("sort_between_name"),
         onLoad = sortable::sortable_js_capture_input("sort_between_name")
       )
     ),
     sortable::sortable_js(
       "sort_within_name",
       options = sortable::sortable_options(
         group = list(
           group = "sortGroup1",
           put = htmlwidgets::JS(
             "function (to) { return to.el.children.length < 1; }"
           ),
           pull = TRUE
         ),
         onSort = sortable::sortable_js_capture_input("sort_within_name"),
         onLoad = sortable::sortable_js_capture_input("sort_within_name")
       )
     ),
     sortable::sortable_js(
       "sort_id_name",
       options = sortable::sortable_options(
         group = list(
           group = "sortGroup1",
           put = htmlwidgets::JS(
             "function (to) { return to.el.children.length < 1; }"
           ),
           pull = TRUE
         ),
         onSort = sortable::sortable_js_capture_input("sort_id_name"),
         onLoad = sortable::sortable_js_capture_input("sort_id_name")
       )
     )
   )
  })

  dv_var <- reactive({
    reactive$data[, input$sort_dv_name]
  })

  between_var <- reactive({
    between_var <- as.factor(reactive$data[, input$sort_between_name])
    # is this necessary?
    if (length(between_var) == 0) between_var <- NULL
    between_var
  })

  within_var <- reactive({
    within_var <- as.factor(reactive$data[, input$sort_within_name])
    if (length(within_var) == 0) within_var <- NULL
    within_var
  })

  id_var <- reactive({
    id_var <- as.factor(reactive$data[, input$sort_id_name])
    if (length(id_var) == 0) id_var <- NULL
    id_var
  })

  # is this necessary?
  # why not set lambda from table?
  # set default lambdas when a new iv is set
  observeEvent(input$sort_between_name, {
    if (length(input$sort_between_name) > 0) {
    between_levels <- stringr::str_sort(unique(between_var()), numeric = TRUE)
    lambda_between <- create_default_lambdas(between_levels)
    reactive$lambda_between <- lambda_between
    } else {
      reactive$lambda_between <- NULL
    }
  })

  # set default lambdas when a new iv is set
  observeEvent(input$sort_within_name, {
    if (length(input$sort_within_name) > 0) {
    within_levels <- stringr::str_sort(unique(within_var()), numeric = TRUE)
    lambda_within <- create_default_lambdas(within_levels)
    reactive$lambda_within <- lambda_within
    } else {
      reactive$lambda_within <- NULL
    }
  })

  # set lambdas when input table changes
  observeEvent(input$hot_lambda_btw, {
    validate(
      need(
        length(input$sort_between_name) > 0,
        "Drag Variable to between."
      )
    )
    df <- rhandsontable::hot_to_r(input$hot_lambda_btw)
    lambda <- as.numeric(df[, 2])
    names(lambda) <- df[, 1]
    # this is why lambda_between needs to be a reactive value
    reactive$lambda_between <- lambda
  })

  # render table
  output$hot_lambda_btw <- rhandsontable::renderRHandsontable({
    validate(
      need(
        length(input$sort_between_name) > 0,
        "Drag Variable to between"
      )
    )

    # is this not just the name of lambda_between?
    lambda_btw <- reactive$lambda_between
    btw <- stringr::str_sort(unique(between_var()), numeric = TRUE)

    df <- data.frame(btw, lambda_btw)
    if (!is.null(df))
      the_tab <- rhandsontable::rhandsontable(df, stretchH = "all",
                                              rowHeaders = NULL)
    rhandsontable::hot_col(the_tab, "btw", readOnly = T)
  })

  # lambda labels within
  output$hot_lambda_wi <- rhandsontable::renderRHandsontable({
    validate(need(input$sort_within_name, "Drag Variable to within."))
    within_levels <- sort(unique(within_var()))
    create_table(within_levels)
  })

  lambda_within <- reactive({
    df <- rhandsontable::hot_to_r(input$hot_lambda_wi)
    lambda <- as.numeric(df[, 2])
    names(lambda) <- df[, 1]
    lambda
  })

  # create table ---------------------------------------------------------------
  output$table_region <- renderPrint({
    # first check that a minimum of parameters is set
    validate(
        need(
          length(input$sort_dv_name) > 0,
          "Drag a variable to Dependent Variable."
        ),
        need(
          length(input$sort_between_name) > 0 |
            length(input$sort_within_name) > 0,
          "Drag at least one Variable to IV (between or within or both)."
        ),
        need(length(reactive$lambda_between) > 0 |
               length(lambda_within()) > 0,
             "Specify Lambdas."
        ),
        if (length(lambda_within()) > 0) {
          need(
            length(input$sort_id_name) > 0,
            "For within designs, an ID variable is required"
          )
        }
    )

    contr <- calc_contrast(
      dv = dv_var(),
      between = between_var(),
      lambda_between = reactive$lambda_between,
      ID = id_var(),
      within = within_var(),
      lambda_within = lambda_within(),
      data = NULL)
    print(contr)
  })
})
