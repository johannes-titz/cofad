#' @importFrom shinyjs show hide
#' @importFrom shinyalert shinyalert
#' @importFrom rhandsontable renderRHandsontable rHandsontableOutput hot_col
#'   hot_to_r
#' @importFrom sortable sortable_js sortable_options sortable_js_capture_input
#' @importFrom htmlwidgets JS
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
      data(list = file, package = "cofad", envir = environment())
      eval(parse(text = paste("data <- ", query[["example"]], sep = "")))
      reactive$data <- data
      shinyjs::show("create_model")
      shinyjs::show("output_region")
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

      # show/hide some panels
      shinyjs::show("create_model")
      shinyjs::hide("help")
      shinyjs::show("output_region")
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
            rhandsontable::rHandsontableOutput(
              "hot_lambda_between",
              width = 250
            )
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
            rhandsontable::rHandsontableOutput("hot_lambda_within", width = 200)
          )
        )
      ),
      # main part of UI finished, now add the sortable_js configuration---------
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

  between_var <- reactive({
    between_var <- as.factor(reactive$data[, input$sort_between_name])
    # this is necessary because of how cofad works, if you drag a variable
    # out of the field, the variable has to change to NULL, otherwise cofad
    # gives an error
    if (length(between_var) == 0) between_var <- NULL
    between_var
  })

  dv_var <- reactive({
    dv_var <- reactive$data[, input$sort_dv_name]
    if (length(dv_var) == 0) dv_var <- NULL
    dv_var
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

  # lambda within --------------------------------------------------------------
  # set default lambdas when a new iv is set, otherwise the last inputs
  # will be kept; if there is no iv set lambdas to NULL, this looks a bit hacky
  # but rhandsontable needs an observeEvent, otherwise it will not work, so it
  # appears the easiest to use a reactive value
  observeEvent(input$sort_within_name, {
    if (length(input$sort_within_name) > 0) {
      within_levels <- stringr::str_sort(unique(within_var()), numeric = TRUE)
      lambda_within <- create_default_lambdas(within_levels)
      reactive$lambda_within <- lambda_within
    } else {
      reactive$lambda_within <- NULL
    }
  })

  # render the rhandsontable
  output$hot_lambda_within <- rhandsontable::renderRHandsontable({
    validate(need(input$sort_within_name, "Drag Variable to within."))
    lambda_within <- reactive$lambda_within
    df <- data.frame("level" = names(lambda_within), lambda = lambda_within)
    if (!is.null(df))
      the_tab <- rhandsontable::rhandsontable(df, stretchH = "all",
                                              rowHeaders = NULL)
    rhandsontable::hot_col(the_tab, "level", readOnly = T)
  })

  # set lambda values when the rhandsontable changes
  observeEvent(input$hot_lambda_within, {
    df <- rhandsontable::hot_to_r(input$hot_lambda_within)
    lambda <- as.numeric(df[, 2])
    names(lambda) <- df[, 1]
    reactive$lambda_within <- lambda
  })

  # lambda between -------------------------------------------------------------
  # same as for within
  observeEvent(input$sort_between_name, {
    if (length(input$sort_between_name) > 0) {
      between_levels <- stringr::str_sort(unique(between_var()), numeric = TRUE)
      lambda_between <- create_default_lambdas(between_levels)
      reactive$lambda_between <- lambda_between
    } else {
      reactive$lambda_between <- NULL
    }
  })

  # render the rhandsontable
  output$hot_lambda_between <- rhandsontable::renderRHandsontable({
    validate(need(input$sort_between_name, "Drag Variable to between."))
    lambda_between <- reactive$lambda_between
    df <- data.frame("level" = names(lambda_between), lambda = lambda_between)
    if (!is.null(df))
      the_tab <- rhandsontable::rhandsontable(df, stretchH = "all",
                                              rowHeaders = NULL)
    rhandsontable::hot_col(the_tab, "level", readOnly = T)
  })

  # set lambda values when the rhandsontable changes
  observeEvent(input$hot_lambda_between, {
    df <- rhandsontable::hot_to_r(input$hot_lambda_between)
    lambda <- as.numeric(df[, 2])
    names(lambda) <- df[, 1]
    reactive$lambda_between <- lambda
  })

  # create output---------------------------------------------------------------
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
             length(reactive$lambda_within) > 0,
           "Specify Lambdas."
      ),
      if (length(reactive$lambda_within) > 0) {
        need(
          length(input$sort_id_name) > 0,
          "For within designs, an ID variable is required"
        )
      }
    )
    # do the analysis
    contr <- calc_contrast(
      dv = dv_var(),
      between = between_var(),
      lambda_between = reactive$lambda_between,
      ID = id_var(),
      within = within_var(),
      lambda_within = reactive$lambda_within,
      data = NULL)
    # print output
    print(contr)
  })
})
