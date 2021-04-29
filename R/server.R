#' @importFrom shinyjs show hide
#' @importFrom shinyalert shinyalert
#' @importFrom rhandsontable renderRHandsontable rHandsontableOutput hot_col
#' @importFrom sortable sortable_js
#' @noRd
myserver <- shinyServer(function(input, output, session) {
  # create reactive variables
  reactive <- reactiveValues()
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
  #         }
  #       }
  #   })
  # upload file area -----------------------------------------------------------
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
    validate(need(!identical(data, reactive$data), "same data"))
    reactive$data <- data

    shinyjs::show("create_model")
    shinyjs::hide("help")
    shinyjs::show("output_region")

    # set reactive values
    reactive$lambda_between <- NULL
    reactive$between_name <- NULL
    reactive$within_name = NULL
    reactive$dv_name <- NULL
    reactive$id_name <- NULL
    reactive$between_var <- NULL
    reactive$within_var <- NULL
    reactive$dv_var <- NULL
    reactive$id_var <- NULL
    reactive$lambda_between <- NULL
    reactive$lambda_within <- NULL
    })
  })
  # USER INTERFACE --------------------
  # this is here because data changes, so variables for first panel are not
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
      column(width=8,
        fluidRow(
          column(width = 4,
        # PANEL DEPENDENT VARIABLES --------------
        tags$div(
          class = "panel panel-default",
          tags$div(
            class = "panel-heading",
            tags$span(class = "glyphicon"),
            "DV"
          ),
          tags$div(class = "panel-body",
                   id = "sort_dv_name")
        )),
        column(width=5,
               tags$div(
          class = "panel panel-default",
          tags$div(
            class = "panel-heading",
            tags$span(class = "glyphicon"),
            "ID Variable"
          ),
          tags$div(class = "panel-body",
                   id = "sort_id_name")
        ))),
      fluidRow(
        # PANEL BETWEEN --------------
        column(width = 4,
        tags$div(
          class = "panel panel-default",
          tags$div(
            class = "panel-heading",
            tags$span(class = "glyphicon"),
            "IV, between"
          ),
          tags$div(class = "panel-body",
                   id = "sort_between_name")
        )),

      # lambda between table ----
        column(width = 5, rhandsontable::rHandsontableOutput("hot_lambda_btw",
                                                             width = 250))
      ), fluidRow(
        # PANEL WITHIN --------------
        column(width = 4,
        tags$div(
          class = "panel panel-default",
          tags$div(
            class = "panel-heading",
            tags$span(class = "glyphicon"),
            "IV, within"
          ),
          tags$div(class = "panel-body",
                   id = "sort_within_name")
        )),

      # lambda within table ----
      #if (length(y()) > 0) {
        column(width = 5, rhandsontable::rHandsontableOutput("hot_lambda_wi", width = 200))#}
      )),
      sortable::sortable_js(
        "sort_variables",
        options = sortable::sortable_options(
          group = list(name = "sortGroup1",
                       put = TRUE),
          sort = FALSE,
          onSort = sortable::sortable_js_capture_input("sort_vars")
        )
      ),
      sortable::sortable_js(
        "sort_dv_name",
        options = sortable::sortable_options(
          group = list(
            group = "sortGroup1",
            put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
            pull = TRUE
          ),
          onSort = sortable::sortable_js_capture_input("sort_dv_name")
        )
      ),
      sortable::sortable_js(
        "sort_between_name",
        options = sortable::sortable_options(
          group = list(
            group = "sortGroup1",
            put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
            pull = TRUE
          ),
          onSort = sortable::sortable_js_capture_input("sort_between_name")
        )
      ),
      sortable::sortable_js(
        "sort_within_name",
        options = sortable::sortable_options(
          group = list(
            group = "sortGroup1",
            put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
            pull = TRUE
          ),
          onSort = sortable::sortable_js_capture_input("sort_within_name")
        )
      ),
      sortable::sortable_js(
        "sort_id_name",
        options = sortable::sortable_options(
          group = list(
            group = "sortGroup1",
            put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
            pull = TRUE
          ),
          onSort = sortable::sortable_js_capture_input("sort_id_name")
        )
      )
    )
  })

  # When we have a between name, set reactive values between_name, between_var
  # and lambda_between, otherwise set them all to NULL
  observeEvent(input$sort_between_name, {
    if (length(input$sort_between_name) > 0) {
    reactive$between_name <- input$sort_between_name
    reactive$between_var <- as.factor(reactive$data[, input$sort_between_name])
    # make a function out of this
    between_levels <- sort(unique(reactive$between_var))
    lambda_between <- 1:length(between_levels)
    lambda_between <- lambda_between - mean(lambda_between)
    names(lambda_between) <- between_levels
    reactive$lambda_between <- lambda_between
    } else {
      reactive$between_name <- NULL
      reactive$between_var <- NULL
      reactive$lambda_between <- NULL
    }
  })
  # When we have a within name, set reactive values within_name, within_var
  # and lambda_within, otherwise set them all to NULL
  observeEvent(input$sort_within_name, {
    if (length(input$sort_within_name) > 0) {
    reactive$within_name <- input$sort_within_name
    reactive$within_var <- as.factor(reactive$data[, input$sort_within_name])
    # make a function out of this
    within_levels <- sort(unique(reactive$within_var))
    lambda_within <- 1:length(within_levels)
    lambda_within <- lambda_within - mean(lambda_within)
    names(lambda_within) <- within_levels
    reactive$lambda_within <- lambda_within
    } else {
      reactive$within_name <- NULL
      reactive$within_var <- NULL
      reactive$lambda_within <- NULL
      reactive$id_name <- NULL
      reactive$id_var <- NULL
    }
  })

  observeEvent(input$sort_dv_name, {
    if (length(input$sort_dv_name) > 0){
    reactive$dv_name <- input$sort_dv_name
    reactive$dv_var <- reactive$data[, input$sort_dv_name]
    } else {
      reactive$dv_name <- NULL
      reactive$dv_var <- NULL
    }
  })

  observeEvent(input$sort_id_name, {
    print(input$sort_id_name)
    if (length(input$sort_id_name) > 0){
    reactive$id_name <- input$sort_id_name
    reactive$id_var <- reactive$data[, input$sort_id_name]
    } else {
       reactive$id_name <- NULL
       reactive$id_var <- NULL
    }
  })

  # debugging
  observe({
  })

  # lambda labels
  observeEvent(input$hot_lambda_btw, {
        validate(need(length(reactive$lambda_between) > 0,
                  "Drag Variable to between."))
    # res <- input$hot_lambda_btw
    # saveRDS(res, file = paste0("../tests/testthat/", input$datafile$name,
    #                            "_hot_lambda_btw.RData"))
    df = rhandsontable::hot_to_r(input$hot_lambda_btw)
    lambda <- as.numeric(df[,2])
    names(lambda) <- df[,1]
    reactive$lambda_between <- lambda
  })

  output$hot_lambda_btw <- rhandsontable::renderRHandsontable({
    #validate(need(length(reactive$lambda_between) > 0, "select btw"))
    # validate(need(reactive$between_name %in% names(reactive$data), "select proper btw"))
    validate(need(length(reactive$lambda_between) > 0,
                  "Drag Variable to between."))
    btw <- sort(unique(reactive$between_var))
    lambda_btw <- reactive$lambda_between
    DF <- data.frame(btw, lambda_btw)
    if (!is.null(DF))
      the_tab <- rhandsontable::rhandsontable(DF, stretchH = "all",
                                              rowHeaders = NULL)
      rhandsontable::hot_col(the_tab, "btw", readOnly = T)
  })

  # lambda labels within
  output$hot_lambda_wi <- rhandsontable::renderRHandsontable({
    validate(need(reactive$within_var, "Drag Variable to within."))
    wi <- sort(unique(reactive$within_var))
    lambda_within <- reactive$lambda_within
    if (is.null(lambda_within)) lambda_within <- 1:length(wi)
    DF <- data.frame(wi, lambda = lambda_within)
    if (!is.null(DF))
      the_tab <- rhandsontable::rhandsontable(DF, stretchH = "all",
                                              rowHeaders = NULL)
      rhandsontable::hot_col(the_tab, "wi", readOnly = T)
  })

  observeEvent(input$hot_lambda_wi, {
    # res <- input$hot_lambda_wi
    # saveRDS(res, file = paste0("../tests/testthat/", input$datafile$name,
    #                            "_hot_lambda_wi.RData"))
    df = rhandsontable::hot_to_r(input$hot_lambda_wi)
    lambda <- as.numeric(df[,2])
    names(lambda) <- df[,1]
    reactive$lambda_within <- lambda
  })

  # create table ---------------------------------------------------------------
  output$table_region <- renderPrint({
    validate(
        need(length(reactive$dv_var) > 0, "Drag a variable to Dependent Variable."),
        need(length(reactive$between_var) > 0 | length(reactive$within_var) > 0,
             "Drag at least one Variable to IV (between or within or both)."),
        need(length(reactive$lambda_between) > 0 | length(reactive$lambda_within) > 0,
             "Specify Lambdas."),
        if (length(reactive$lambda_within) > 0) need(reactive$id_var, "For within designs, an ID variable is required"),
        if (length(reactive$id_var) > 0) need(reactive$within_var, "If you use an ID variable, cofad assumes you have a within-design, so please specify the within-variable."),
        if (length(reactive$between_var > 0)) need(reactive$lambda_between, "Specify b")
      )

   contr <- calc_contrast(
   dv = reactive$dv_var,
   between = reactive$between_var,
   lambda_between = reactive$lambda_between,
   ID = reactive$id_var,
   within = reactive$within_var,
   lambda_within = reactive$lambda_within,
   data = NULL)
   print(contr)
  })
})
