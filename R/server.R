#' @importFrom shinyjs show hide
#' @importFrom rhandsontable renderRHandsontable rHandsontableOutput hot_col hot_to_r
#' @noRd
myserver <- shinyServer(function(input, output, session) {
  reactive <- reactiveValues(data_version = 0)
  shinyjs::hide("create_model")
  shinyjs::show("help")
  shinyjs::hide("output_region")

  set_data <- function(data) {
    reactive$data <- data
    reactive$varnames <- names(data)
    numeric_names <- names(data)[vapply(data, is.numeric, logical(1))]
    default_dv <- if (length(numeric_names)) numeric_names[[1]] else ""
    suggestion <- tryCatch(
      detect_design(data),
      error = function(e) NULL
    )
    if (!is.null(suggestion)) {
      default_dv <- suggestion$dv_name
      reactive$design_suggestion <- suggestion
    } else {
      reactive$design_suggestion <- NULL
    }
    reactive$model_spec <- c(
      dv_name = default_dv,
      between_name = if (is.null(suggestion)) "" else suggestion$between_name,
      within_name = if (is.null(suggestion)) "" else suggestion$within_name,
      id_name = if (is.null(suggestion)) "" else suggestion$id_name
    )
    reactive$lambda_between <- NULL
    reactive$lambda_within <- NULL
    reactive$data_version <- reactive$data_version + 1
    shinyjs::show("create_model")
    shinyjs::show("output_region")
    shinyjs::hide("help")
  }

  # Example data sets ----------------------------------------------------------
  observe({
    query <- parseQueryString(session$clientData$url_search)
    example <- query[["example"]]

    if (!is.null(example)) {
      validate(need(
        length(example) == 1 && grepl("^[A-Za-z][A-Za-z0-9_.]*$", example),
        "Invalid example data set name."
      ))
      example_data <- load_cofad_example(example)
      validate(need(!is.null(example_data), "Example data set not found."))
      set_data(example_data)
    }
  })

  observeEvent(input$datafile, {
    withProgress(message = "Loading data", value = 0, {
      req(input$datafile)
      data <- load_data(input$datafile)
      validate(need(is.data.frame(data), "The file did not contain tabular data."))
      set_data(data)
    })
  })

  # Model input ---------------------------------------------------------------
  output$variables <- renderUI({
    req(reactive$data)
    default_dv <- isolate(reactive$model_spec[["dv_name"]])

    tagList(
      tags$p(
        "Suggested roles are inferred from replication and nesting. Please ",
        "verify them, especially for incomplete or unusual designs. Use NONE ",
        "for roles that are not part of the design."
      ),
      if (is.null(reactive$design_suggestion)) {
        tags$p(
          class = "cofad-note cofad-note-warning",
          "Automatic detection could not run. Select the dependent variable ",
          "and design roles manually in the table below."
        )
      } else if (identical(
        reactive$design_suggestion$design, "undetermined"
      )) {
        tags$p(
          class = "cofad-note cofad-note-warning",
          "Automatic detection was inconclusive. Select the dependent ",
          "variable and design roles manually in the table below."
        )
      } else {
        tags$p(
          class = "cofad-note",
          paste0(
            "Suggested ", reactive$design_suggestion$design, " design (",
            round(100 * reactive$design_suggestion$confidence),
            "% structural confidence). Every suggestion remains editable."
          )
        )
      },
      rhandsontable::rHandsontableOutput("hot_model", height = 150),
      # These canonical inputs keep old bookmarks and automated clients working.
      tags$div(
        style = "display:none;",
        selectInput("dv_name", "DV", choices = reactive$varnames,
                    selected = default_dv),
        selectInput(
          "between_name", "IV between",
          choices = c("NONE" = "", reactive$varnames),
          selected = isolate(reactive$model_spec[["between_name"]])
        ),
        selectInput(
          "within_name", "IV within",
          choices = c("NONE" = "", reactive$varnames),
          selected = isolate(reactive$model_spec[["within_name"]])
        ),
        selectInput(
          "id_name", "ID", choices = c("NONE" = "", reactive$varnames),
          selected = isolate(reactive$model_spec[["id_name"]])
        )
      ),
      tags$hr(),
      tags$h4("Between-subjects contrast weights"),
      rhandsontable::rHandsontableOutput("hot_lambda_between", height = 180),
      tags$h4("Within-subjects contrast weights"),
      rhandsontable::rHandsontableOutput("hot_lambda_within", height = 180)
    )
  })

  output$hot_model <- rhandsontable::renderRHandsontable({
    req(reactive$data, reactive$model_spec)
    selected <- unname(reactive$model_spec)
    selected[is.na(selected) | !nzchar(selected)] <- "NONE"
    model <- data.frame(
      role = c("Dependent variable", "Between-subjects factor",
               "Within-subjects factor", "Participant ID"),
      variable = selected,
      check.names = FALSE
    )
    hot <- rhandsontable::rhandsontable(
      model, stretchH = "all", rowHeaders = NULL
    )
    hot <- rhandsontable::hot_col(hot, "role", readOnly = TRUE)
    rhandsontable::hot_col(
      hot, "variable", type = "dropdown",
      source = c("NONE", reactive$varnames), strict = TRUE,
      allowInvalid = FALSE
    )
  })

  observeEvent(input$hot_model, {
    model <- rhandsontable::hot_to_r(input$hot_model)
    req(nrow(model) == 4)
    selected <- as.character(model$variable)
    selected[selected == "NONE"] <- ""
    ids <- c("dv_name", "between_name", "within_name", "id_name")
    current <- c(
      input$dv_name, input$between_name, input$within_name, input$id_name
    )
    names(selected) <- ids
    if (!identical(selected, reactive$model_spec)) {
      reactive$model_spec <- selected
    }
    for (i in seq_along(ids)) {
      if (!identical(selected[[i]], current[[i]])) {
        updateSelectInput(session, ids[[i]], selected = selected[[i]])
      }
    }
  }, ignoreInit = TRUE)

  # Keep legacy/bookmarked input values synchronized with the model table.
  observeEvent(
    list(input$dv_name, input$between_name, input$within_name, input$id_name),
    {
      req(reactive$data, input$dv_name)
      selected <- c(
        dv_name = input$dv_name,
        between_name = input$between_name,
        within_name = input$within_name,
        id_name = input$id_name
      )
      if (!identical(selected, reactive$model_spec)) {
        reactive$model_spec <- selected
      }
    },
    ignoreInit = TRUE
  )

  within_var <- reactive({
    req(input$within_name)
    as.factor(reactive$data[, input$within_name])
  })

  observeEvent(list(input$within_name, reactive$data_version), {
    if (is.null(input$within_name) || !nzchar(input$within_name)) {
      reactive$lambda_within <- NULL
    } else {
      within_levels <- levels(within_var())
      reactive$lambda_within <- create_default_lambdas(within_levels)
    }
  }, ignoreInit = FALSE)

  output$hot_lambda_within <- rhandsontable::renderRHandsontable({
    validate(need(
      input$within_name,
      "Select a within-subjects factor in the model table first."
    ))
    df <- prepare_table(
      reactive$lambda_within, reactive$data[, input$within_name]
    )
    hot <- rhandsontable::rhandsontable(
      df, stretchH = "all", rowHeaders = NULL
    )
    rhandsontable::hot_col(hot, c("level", "n"), readOnly = TRUE)
  })

  observeEvent(input$hot_lambda_within, {
    df <- rhandsontable::hot_to_r(input$hot_lambda_within)
    lambda <- as.numeric(df[, 2])
    names(lambda) <- df[, 1]
    reactive$lambda_within <- lambda
  })

  observeEvent(list(input$between_name, reactive$data_version), {
    if (is.null(input$between_name) || !nzchar(input$between_name)) {
      reactive$lambda_between <- NULL
    } else {
      between_levels <- levels(as.factor(reactive$data[, input$between_name]))
      reactive$lambda_between <- create_default_lambdas(between_levels)
    }
  }, ignoreInit = FALSE)

  output$hot_lambda_between <- rhandsontable::renderRHandsontable({
    validate(need(
      input$between_name,
      "Select a between-subjects factor in the model table first."
    ))
    df <- prepare_table(
      reactive$lambda_between, reactive$data[, input$between_name]
    )
    hot <- rhandsontable::rhandsontable(
      df, stretchH = "all", rowHeaders = NULL
    )
    rhandsontable::hot_col(hot, c("level", "n"), readOnly = TRUE)
  })

  observeEvent(input$hot_lambda_between, {
    df <- rhandsontable::hot_to_r(input$hot_lambda_between)
    lambda <- as.numeric(df[, 2])
    names(lambda) <- df[, 1]
    reactive$lambda_between <- lambda
  })

  selected_variable <- function(name, factor = FALSE) {
    if (is.null(name) || !nzchar(name)) return(NULL)
    value <- reactive$data[, name]
    if (factor) as.factor(value) else value
  }

  analysis <- reactive({
    req(reactive$data, input$dv_name)
    validate(
      need(is.numeric(reactive$data[, input$dv_name]),
           "The dependent variable must be numeric."),
      need(nzchar(input$between_name) || nzchar(input$within_name),
           "Specify at least one between- or within-subjects factor."),
      need(length(reactive$lambda_between) > 0 ||
             length(reactive$lambda_within) > 0,
           "Specify contrast weights."),
      if (nzchar(input$within_name)) {
        need(nzchar(input$id_name),
             "A participant ID is required for within-subjects designs.")
      },
      if (!nzchar(input$within_name) && nzchar(input$id_name)) {
        need(FALSE,
             "Select a within-subjects factor when a participant ID is used.")
      }
    )

    calc_contrast(
      dv = reactive$data[, input$dv_name],
      between = selected_variable(input$between_name, factor = TRUE),
      lambda_between = reactive$lambda_between,
      id = selected_variable(input$id_name, factor = TRUE),
      within = selected_variable(input$within_name, factor = TRUE),
      lambda_within = reactive$lambda_within,
      data = NULL
    )
  })

  output$table_region <- renderText({
    contr <- analysis()

    if (length(reactive$lambda_between) &&
        sum(reactive$lambda_between) != 0) {
      showNotification(
        "The between-subjects weights do not sum to zero; cofad centers them ",
        "automatically.",
        type = "warning", id = "lambda_btw", duration = NULL,
        closeButton = TRUE
      )
    } else {
      removeNotification(id = "lambda_btw")
    }
    if (length(reactive$lambda_within) &&
        sum(reactive$lambda_within) != 0) {
      showNotification(
        "The within-subjects weights do not sum to zero; cofad centers them ",
        "automatically.",
        type = "warning", id = "lambda_wi", duration = NULL,
        closeButton = TRUE
      )
    } else {
      removeNotification(id = "lambda_wi")
    }

    report <- paste(utils::capture.output(print(contr)), collapse = "\n")
    between_result <- inherits(contr, "cofad_bw") || inherits(contr, "cofad_mx")

    as.character(tagList(
      div(
        class = "cofad-results",
        tags$h4("Report"),
        tags$pre(report, class = "cofad-report"),
        if (between_result) {
          tagList(
            tags$h4("Variance decomposition (F table)"),
            cofad_html_table(detailed_f_table(contr)),
            tags$p(
              class = "cofad-note",
              "The contrast is a one-degree-of-freedom component of the ",
              "overall between-group variation. F-table p values are ",
              "non-directional.",
              if (inherits(contr, "cofad_mx")) {
                paste0(
                  " For mixed designs, this decomposition concerns the ",
                  "participants' within-contrast L values."
                )
              }
            ),
            tags$h4("Effect sizes and explained proportions"),
            cofad_html_table(detailed_effect_table(contr)),
            tags$h4("Partition of total variation")
          )
        } else {
          tagList(
            tags$h4("Effect sizes"),
            cofad_html_table(detailed_effect_table(contr)),
            tags$p(
              class = "cofad-note",
              "A within-subjects contrast is tested through participants' ",
              "L values, so the between-subjects F-table partition does not ",
              "apply."
            )
          )
        }
      )
    ))
  })

  output$variance_partition <- shiny::renderPlot({
    contr <- analysis()
    if (inherits(contr, "cofad_bw") || inherits(contr, "cofad_mx")) {
      plot_variance_partition(contr)
    } else {
      graphics::plot.new()
    }
  }, res = 96)

  output$citation_region <- renderUI({
    tagList(tags$hr(), HTML(cite()))
  })
})
