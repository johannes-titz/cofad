#' @importFrom shinyjs show hide
#' @importFrom shinyalert shinyalert
#' @importFrom rhandsontable renderRHandsontable rHandsontableOutput hot_col
#' @importFrom sortable sortable_js
#' @noRd
myserver <- shinyServer(function(input, output, session) {
  # create reactive variables
  reactive <- reactiveValues(data_lambda = data.frame(),
                             data_lambda_within = data.frame(),
                             data = data.frame(),
                             r_mdl_formula = "")
  lambda_between = reactive({
    if (!is.null(input$hot_lambda_btw)) {
      df = rhandsontable::hot_to_r(input$hot_lambda_btw)
      lambda <- df[,2]
      names(lambda) <- df[,1]
      reactive$data_lambda = df
      lambda
    }
  })
  lambda_within = reactive({
    if (!is.null(input$hot_lambda_wi)) {
      df = rhandsontable::hot_to_r(input$hot_lambda_wi)
      lambda <- df[,2]
      names(lambda) <- df[,1]
      reactive$data_lambda_within = df
      lambda
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
    reactive$data <- data
    reactive$names <- names(data)
    shinyjs::show("create_model")
  #           shinyjs::show("reactive_mode_area")
  #           shinyjs::hide("display_model")
  #           shinyjs::hide("output_region")
    shinyjs::hide("help")
    shinyjs::show("output_region")
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
                   id = "sort_id")
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
                   id = "sort3")
        )),

      # lambda between table ----
      #if (length(y()) > 0) {
        column(width = 5, rhandsontable::rHandsontableOutput("hot_lambda_btw", width = 250))#}
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
                   id = "sort_within")
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
        "sort3",
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
        "sort_within",
        options = sortable::sortable_options(
          group = list(
            group = "sortGroup1",
            put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
            pull = TRUE
          ),
          onSort = sortable::sortable_js_capture_input("sort_within")
        )
      ),
      sortable::sortable_js(
        "sort_id",
        options = sortable::sortable_options(
          group = list(
            group = "sortGroup1",
            put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
            pull = TRUE
          ),
          onSort = sortable::sortable_js_capture_input("sort_id")
        )
      )
    )
  })

  id <- reactive({
    if (length(input$sort_id) > 0){
      sort_id <- input$sort_id
      #if (is.character(dv_name)) dv_name
      reactive$data[,sort_id]
    } else NULL
  })

  dv <- reactive({
    if (length(input$sort_dv_name) > 0){
      dv_name <- input$sort_dv_name
      #if (is.character(dv_name)) dv_name
      reactive$data[,dv_name]
    } else NULL
  })

  between <- reactive({
    if (length(input$sort_between_name) > 0){
      between_var_name <- input$sort_between_name
      as.factor(reactive$data[, between_var_name])
    } else NULL
  })

  within <- reactive({
    if (length(input$sort_within) > 0){
      within_var_name <- input$sort_within
      as.factor(reactive$data[, within_var_name])
    } else NULL
  })

  # lambda labels
  output$hot_lambda_btw <- rhandsontable::renderRHandsontable({
    validate(need(between(), "Drag Variable to between."))
    btw <- sort(unique(between()))
    DF <- data.frame(btw, lambda = 1:length(btw))
    if (!is.null(DF))
      the_tab <- rhandsontable::rhandsontable(DF, stretchH = "all")
      rhandsontable::hot_col(the_tab, "btw", readOnly = T)
  })

  # lambda labels within
  output$hot_lambda_wi <- rhandsontable::renderRHandsontable({
    validate(need(within(), "Drag Variable to within."))
    wi <- sort(unique(within()))
    DF <- data.frame(wi, lambda = 1:length(wi))
    if (!is.null(DF))
      the_tab <- rhandsontable::rhandsontable(DF, stretchH = "all")
      rhandsontable::hot_col(the_tab, "wi", readOnly = T)
  })

  # create table ---------------------------------------------------------------
  output$table_region <- renderPrint({
    validate(
        need(dv(), "Drag a variable to Dependent Variable."),
        need(length(between()) > 0 | length(within()) > 0,
             "Drag at least one Variable to IV (between or within or both)."),
        need(length(lambda_between()) > 0 | length(lambda_within()) > 0,
             "Specify Lambdas.")#,
        #
        #need(length(id()) > 0 | is.null(id()), "id length 0")
      )
   #dat <- reactive$data[, c(dv_name(), between_name())]
   #names(dat) <- c("dv_name", "between_name")

   # ID is needed for within, right?
   #dat$between_name<- as.factor(dat$between_name)
   contr <- calc_contrast(
   dv = dv(),
   between = between(),
   lambda_between = lambda_between(),
   ID = id(),
   within = within(),
   lambda_within = lambda_within(),
   data = NULL)
   print(contr)
  })
})
