#' @importFrom shinyjs show hide
#' @importFrom rhandsontable renderRHandsontable rHandsontableOutput hot_col
#'   hot_to_r
#' @noRd
myserver <- shinyServer(function(input, output, session) {
  # create reactive variables
  reactive <- reactiveValues()
  shinyjs::hide("create_model")
  shinyjs::show("help")
  shinyjs::hide("output_region")

  # example data sets ----------------------------------------------------------
  observe({
    query <- parseQueryString(session$clientData$url_search)

    if (!is.null(query[["example"]])) {
      file <- paste(query[["example"]], sep = "")
      data(list = file, package = "cofad", envir = environment())
      eval(parse(text = paste("data <- ", query[["example"]], sep = "")))
      reactive$data <- data
      reactive$varnames <- names(data)
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
      reactive$varnames <- names(data)

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
    div(
    fluidRow(column(width = 3,
             radioButtons("dv_name", label = "DV", reactive$varnames))),
    fluidRow(
      column(width = 3,
      radioButtons("between_name", label = "IV between",
                   choiceNames = c("NONE", reactive$varnames),
                   choiceValues = c("", reactive$varnames))),
      column(width = 9,
      rhandsontable::rHandsontableOutput(
        "hot_lambda_between", width = 250
      ))),
    br(),
    fluidRow(
          # lambda within table ----
          column(
            width = 3,
            radioButtons("within_name", label = "IV within",
                         choiceNames = c("NONE", reactive$varnames),
                         choiceValues = c("", reactive$varnames)),
             radioButtons("id_name", label = "ID for within",
                         choiceNames = c("NONE", reactive$varnames),
                         choiceValues = c("", reactive$varnames))

          ),
      column(width = 9,
             #sapply(1:3, function(x) numericInput(x, x, x-mean(1:3)))
             rhandsontable::rHandsontableOutput("hot_lambda_within", width = 250)

      ),
    )
    )
  })

  within_var <- reactive({
    req(input$within_name)
    within_var <- as.factor(reactive$data[, input$within_name])
    within_var
  })

  id_var <- reactive({
    req(input$id_name)
    id_var <- as.factor(reactive$data[, input$id_name])
    id_var
  })

  observeEvent(input$reset, {
    updateSelectInput(session, "dv_name", selected = "")
  })

  # lambda within --------------------------------------------------------------
  # set default lambdas when a new iv is set, otherwise the last inputs
  # will be kept; if there is no iv set lambdas to NULL, this looks a bit hacky
  # but rhandsontable needs an observeEvent, otherwise it will not work, so it
  # appears the easiest to use a reactive value
  observeEvent(input$within_name, {
    if (length(input$within_name) > 0) {
      within_levels <- stringr::str_sort(unique(within_var()), numeric = TRUE)
      lambda_within <- create_default_lambdas(within_levels)
      reactive$lambda_within <- lambda_within
    } else {
      reactive$lambda_within <- NULL
    }
  })

  # render the rhandsontable
  output$hot_lambda_within <- rhandsontable::renderRHandsontable({
    validate(need(input$within_name, "To specify lambdas within, first select the within variable."))
    lambda_within <- reactive$lambda_within
    df <- prepare_table(lambda_within, within_var())
    if (!is.null(df))
      the_tab <- rhandsontable::rhandsontable(df, stretchH = "all",
                                              rowHeaders = NULL)
    rhandsontable::hot_col(the_tab, c("level", "n"), readOnly = T)
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
  observeEvent(input$between_name, {
    req(input$between_name)
      between_levels <- stringr::str_sort(unique(reactive$data[, input$between_name],
                                                 numeric = TRUE))
      lambda_between <- create_default_lambdas(between_levels)
      reactive$lambda_between <- lambda_between
  })

  # render the rhandsontable
  output$hot_lambda_between <- rhandsontable::renderRHandsontable({
    validate(need(input$between_name, "To specify lambdas between, first select the between variable."))
    lambda_between <- reactive$lambda_between
    df <- prepare_table(lambda_between, input$between_name)
    if (!is.null(df))
      the_tab <- rhandsontable::rhandsontable(df, stretchH = "all",
                                              rowHeaders = NULL)
    rhandsontable::hot_col(the_tab, c("level", "n"), readOnly = T)
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
      need(input$dv_name, "Dependent variable is missing."),
      need(length(input$between_name) > 0 | length(input$within_name) > 0,
        "Specify at least one IV Variable (between or within or both)."
      ),
      need(length(reactive$lambda_between) > 0 |
           length(reactive$lambda_within) > 0,
           "Specify Lambdas."
      ),
      if (length(reactive$lambda_within) > 0) {
        need(input$id_name,
          "For within designs, an ID variable is required"
        )
      }
    )
    if (input$between_name %in% c("", "NONE", NULL)) {
      btw <- NULL
      } else {
        btw <- reactive$data[, input$between_name]
        }
    if (input$within_name %in% c("", "NONE", NULL)) {
      within <- NULL
      } else {
        within <- reactive$data[, input$within_name]
        }
    if (input$id_name %in% c("", "NONE", NULL)) {
      id <- NULL
      } else {
        id <- reactive$data[, input$id_name]
        }

    contr <- calc_contrast(
      dv = reactive$data[, input$dv_name],
      between = btw,
      lambda_between = reactive$lambda_between,
      id = id,
      within = within,
      lambda_within = reactive$lambda_within,
      data = NULL
    )

    if (sum(reactive$lambda_between) != 0) {
      showNotification(
        "Your between lambdas do not sum up to 0. They are automatically centered.",
        type = "warning", id = "lambda_btw", duration = NULL,
        closeButton = TRUE)
    } else {
      removeNotification(id = "lambda_btw")
    }
    if (sum(reactive$lambda_within) != 0) {
      showNotification("Your within lambdas do not sum up to 0. They are automatically centered.", type = "warning", id = "lambda_wi", duration = NULL,
        closeButton = TRUE)
    } else {
      removeNotification(id = "lambda_wi")
    }
    # print output

    output <- utils::capture.output(print(contr))
    output <- gsub("F\\(", "<i>F</i>\\(", output)
    output <- gsub("t\\(", "<i>t</i>\\(", output)
    output <- gsub("p =", "<i>p </i>=", output)
    output <- gsub("r_effectsize", "<i>r</i><sub>effect size</sub>", output)
    output <- gsub("g_effectsize", "<i>g</i><sub>effect size</sub>", output)
    HTML(c(output, "<br><br>", cite()))
  })
})
