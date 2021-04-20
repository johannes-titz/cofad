
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
                             data_lambda_within = data.frame(),
                             data = data.frame(),
                             r_mdl_formula = "")
  data_lambda = reactive({
    if (!is.null(input$hot_lambda_btw)) {
      df = hot_to_r(input$hot_lambda_btw)
      reactive$data_lambda = df
      df
    }
  })
  data_lambda_within = reactive({
    if (!is.null(input$hot_lambda_wi)) {
      df = hot_to_r(input$hot_lambda_wi)
      reactive$data_lambda_within = df
      df
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
      column(width=7,
        fluidRow(
          column(width = 7,
        # PANEL DEPENDENT VARIABLES --------------
        tags$div(
          class = "panel panel-default",
          tags$div(
            class = "panel-heading",
            tags$span(class = "glyphicon"),
            "Dependent Variable (drag here)"
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
            "ID Variable (drag here)"
          ),
          tags$div(class = "panel-body",
                   id = "sort_id")
        ))),
      fluidRow(
        # PANEL BETWEEN --------------
        column(width = 7,
        tags$div(
          class = "panel panel-default",
          tags$div(
            class = "panel-heading",
            tags$span(class = "glyphicon"),
            "Independent Variable, between (drag here)"
          ),
          tags$div(class = "panel-body",
                   id = "sort3")
        )),

      # lambda between table ----
      #if (length(y()) > 0) {
        column(width = 5, rHandsontableOutput("hot_lambda_btw", width = 200))#}
      ), fluidRow(
        # PANEL WITHIN --------------
        column(width = 7,
        tags$div(
          class = "panel panel-default",
          tags$div(
            class = "panel-heading",
            tags$span(class = "glyphicon"),
            "Independent Variable, within (drag here)"
          ),
          tags$div(class = "panel-body",
                   id = "sort_within")
        )),

      # lambda between table ----
      #if (length(y()) > 0) {
        column(width = 5, rHandsontableOutput("hot_lambda_wi", width = 200))#}
      )),
      sortable_js(
        "sort_variables",
        options = sortable_options(
          group = list(name = "sortGroup1",
                       put = TRUE),
          sort = FALSE,
          onSort = sortable_js_capture_input("sort_vars")
        )
      ),
      sortable_js(
        "sort_dv_name",
        options = sortable_options(
          group = list(
            group = "sortGroup1",
            put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
            pull = TRUE
          ),
          onSort = sortable_js_capture_input("sort_dv_name")
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
          onSort = sortable_js_capture_input("sort_between_name")
        )
      ),
      sortable_js(
        "sort_within",
        options = sortable_options(
          group = list(
            group = "sortGroup1",
            put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
            pull = TRUE
          ),
          onSort = sortable_js_capture_input("sort_within")
        )
      ),
      sortable_js(
        "sort_id",
        options = sortable_options(
          group = list(
            group = "sortGroup1",
            put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
            pull = TRUE
          ),
          onSort = sortable_js_capture_input("sort_id")
        )
      )
    )
  })

  dv_name <- reactive({
    dv_name <- input$sort_dv_name
    if (is.character(dv_name)) dv_name %>% trimws()
  })

  between_name<- reactive({
    res <- input$sort_between_name %>% trimws()
    res
  })

  within_var_name <- reactive({
    input$sort_within %>% trimws()
  })

  # lambda labels
  output$hot_lambda_btw <- renderRHandsontable({
    validate(need(between_name(), ""))
    btw <- sort(unique(reactive$data[, c(between_name())]))
    DF <- data.frame(btw, lambda = 1:length(btw))
    if (!is.null(DF))
      rhandsontable(DF, stretchH = "all")
  })

  # lambda labels within
  output$hot_lambda_wi <- renderRHandsontable({
    validate(need(within_var_name(), ""))
    between <- sort(unique(reactive$data[, c(within_var_name())]))
    DF <- data.frame(between, lambda = 1:length(between))
    if (!is.null(DF))
      rhandsontable(DF, stretchH = "all")
  })

  # create table ---------------------------------------------------------------
  output$table_region <- renderPrint({
    validate(
        need(dv_name(), "Drag a variable to Dependent Variable."),
        need(length(between_name()) > 0 | length(within_var_name() > 0),
             "Drag at least one Variable to Independent Variable (between or within or both).")
      )

   dat <- reactive$data[, c(dv_name(), between_name())]
   names(dat) <- c("dv_name", "between_name")

   # ID is needed for within, right?
   dat$between_name<- as.factor(dat$between_name)
   data_lambda <- data_lambda()
   lambda_between <- data_lambda[,2]
   names(lambda_between) <- data_lambda[,1]
   contr_bw <- calc_contrast(
   dv = dv_name,
   between = between_name,
   lambda_between = lambda_between,
   data = dat)
   print(contr_bw)
  })
})
