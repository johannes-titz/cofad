#' @importFrom shinyjs show hide
#' @importFrom rhandsontable renderRHandsontable rHandsontableOutput hot_col hot_to_r
#' @noRd
myserver <- shinyServer(function(input, output, session) {
  reactive <- reactiveValues(data_version = 0)
  shinyjs::hide("create_model")
  shinyjs::show("help")
  shinyjs::hide("output_region")
  shinyjs::hide("code_region")

  set_data <- function(data, example_spec = NULL, example_name = NULL) {
    reactive$data <- data
    reactive$example_spec <- example_spec
    reactive$example_name <- example_name
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
    reactive$model_spec <- if (!is.null(example_spec)) {
      example_spec$roles
    } else {
      c(
        dv_name = default_dv,
        between_name = if (is.null(suggestion)) "" else suggestion$between_name,
        within_name = if (is.null(suggestion)) "" else suggestion$within_name,
        id_name = if (is.null(suggestion)) "" else suggestion$id_name
      )
    }
    is_mixed <- nzchar(reactive$model_spec[["between_name"]]) &&
      nzchar(reactive$model_spec[["within_name"]])
    reactive$mixed_effect <- if (is_mixed && !is.null(example_spec) &&
        is.null(example_spec$between)) {
      "within"
    } else {
      "interaction"
    }
    reactive$compare_competing <- isTRUE(example_spec$competing)
    reactive$lambda_between <- NULL
    reactive$lambda_between_rival <- NULL
    reactive$lambda_within <- NULL
    reactive$lambda_within_rival <- NULL
    reactive$data_version <- reactive$data_version + 1
    shinyjs::show("create_model")
    shinyjs::show("output_region")
    shinyjs::show("code_region")
    compare_default <- isTRUE(example_spec$competing)
    mixed_effect_default <- reactive$mixed_effect
    session$onFlushed(function() {
      updateCheckboxInput(
        session, "compare_competing", value = compare_default
      )
      updateRadioButtons(
        session, "mixed_effect", selected = mixed_effect_default
      )
    }, once = TRUE)
  }

  load_example <- function(example) {
    if (identical(isolate(reactive$example_name), example) &&
        !is.null(isolate(reactive$data))) {
      return(invisible(FALSE))
    }
    example_data <- load_cofad_example(example)
    validate(need(!is.null(example_data), "Example data set not found."))
    set_data(example_data, cofad_example_spec(example), example)
    invisible(TRUE)
  }

  # Example data sets ----------------------------------------------------------
  output$example_description <- renderUI({
    req(input$example_dataset)
    tags$p(class = "cofad-note", cofad_example_description(input$example_dataset))
  })

  observeEvent(input$example_dataset, {
    req(nzchar(input$example_dataset))
    load_example(input$example_dataset)
  }, ignoreInit = FALSE)

  observe({
    query <- parseQueryString(session$clientData$url_search)
    example <- query[["example"]]

    if (!is.null(example)) {
      validate(need(
        length(example) == 1 && grepl("^[A-Za-z][A-Za-z0-9_.]*$", example),
        "Invalid example data set name."
      ))
      updateSelectInput(session, "example_dataset", selected = example)
      load_example(example)
    }
  })

  session$onFlushed(function() {
    if (is.null(isolate(reactive$data))) {
      example <- cofad_default_example()
      updateSelectInput(session, "example_dataset", selected = example)
      load_example(example)
    }
  }, once = TRUE)

  observeEvent(input$datafile, {
    withProgress(message = "Loading data", value = 0, {
      req(input$datafile)
      data <- load_data(input$datafile)
      validate(need(is.data.frame(data), "The file did not contain tabular data."))
      updateSelectInput(session, "example_dataset", selected = "")
      set_data(data)
    })
  })

  # Model input ---------------------------------------------------------------
  output$variables <- renderUI({
    req(reactive$data)

    tagList(
      tags$p(
        if (is.null(reactive$example_spec)) {
          paste(
            "Suggested roles are inferred from replication and nesting.",
            "Please verify them, especially for incomplete or unusual designs."
          )
        } else {
          paste(
            "The model roles and planned weights below reproduce the example's",
            "book, paper, or documented study hypothesis."
          )
        },
        " Use NONE for roles that are not part of the design."
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
      tags$div(
        class = "cofad-hot-wrap",
        rhandsontable::rHandsontableOutput(
          "hot_model", width = "auto", height = "130px"
        )
      ),
      checkboxInput(
        "compare_competing",
        label = tags$span(
          class = "cofad-help-tooltip",
          title = paste(
            "Adds a rival column to each active contrast table.",
            "cofad z-standardizes both vectors and analyzes favored minus",
            "rival weights. The initial rival reverses the favored weights,",
            "so enabling the option preserves the current test until you edit",
            "it. Turning this off hides the rival columns and restores the",
            "favored weights as ordinary single contrasts."
          ),
          "Compare two competing contrasts"
        ),
        value = isTRUE(reactive$compare_competing)
      ),
      # These canonical inputs keep old bookmarks and automated clients working.
      tags$div(
        style = "display:none;",
        selectInput(
          "dv_name", "DV", choices = reactive$varnames,
          selected = isolate(reactive$model_spec[["dv_name"]])
        ),
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
      conditionalPanel(
        condition = "input.between_name != '' && input.within_name != ''",
        radioButtons(
          "mixed_effect", "Mixed-design contrast to test",
          choiceNames = list(
            tags$span(
              class = "cofad-help-tooltip",
              title = paste(
                "Tests the product of the planned between- and",
                "within-subjects weights: whether the within-subject pattern",
                "differs across groups as predicted by the between contrast."
              ),
              "Between \u00d7 within contrast"
            ),
            tags$span(
              class = "cofad-help-tooltip",
              title = paste(
                "Tests the planned within-subjects contrast averaged across",
                "groups. Between-subjects contrast weights are not used; the",
                "between factor is retained for grouping and error pooling."
              ),
              "Within contrast averaged across groups"
            )
          ),
          choiceValues = c("interaction", "within"),
          selected = isolate(reactive$mixed_effect), inline = TRUE
        ),
        tags$p(
          class = "cofad-note",
          "Choose between \u00d7 within when the within-subject pattern is ",
          "predicted to differ across groups. Choose the averaged within ",
          "contrast when the within-subject pattern is the target across all ",
          "groups; the between factor is then used for grouping and error ",
          "pooling."
        )
      ),
      conditionalPanel(
        condition = paste(
          "input.between_name != '' && (input.within_name == '' ||",
          "input.mixed_effect == 'interaction')"
        ),
        tags$h4("Between-subjects contrast weights"),
        tags$div(
          class = "cofad-hot-wrap",
          rhandsontable::rHandsontableOutput(
            "hot_lambda_between", width = "auto", height = "130px"
          )
        )
      ),
      conditionalPanel(
        condition = "input.within_name != ''",
        tags$h4("Within-subjects contrast weights"),
        tags$div(
          class = "cofad-hot-wrap",
          rhandsontable::rHandsontableOutput(
            "hot_lambda_within", width = "auto", height = "130px"
          )
        ),
        radioButtons(
          "within_score", "Participant-level within score",
          choices = c(
            "L score (retains magnitude)" = "L",
            "r score (pattern fit)" = "r"
          ),
          selected = "L", inline = TRUE
        ),
        tags$p(
          class = "cofad-note",
          "Choose L when response magnitude matters; choose r when agreement ",
          "with the predicted pattern matters. Decide before inspecting results."
        )
      )
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
    variable_width <- max(
      120, min(235, 30 + 8 * max(nchar(c("NONE", reactive$varnames))))
    )
    table_width <- 190 + variable_width + 25
    hot <- rhandsontable::rhandsontable(
      model, stretchH = "none", rowHeaders = NULL, width = table_width,
      height = 126, renderAllColumns = TRUE
    )
    hot <- rhandsontable::hot_col(hot, "role", readOnly = TRUE, width = 190)
    rhandsontable::hot_col(
      hot, "variable", type = "dropdown",
      source = c("NONE", reactive$varnames), strict = TRUE,
      allowInvalid = FALSE, width = variable_width
    )
  })

  observeEvent(input$hot_model, {
    model <- rhandsontable::hot_to_r(input$hot_model)
    req(nrow(model) == 4)
    selected <- as.character(model$variable)
    selected[selected == "NONE"] <- ""
    ids <- c("dv_name", "between_name", "within_name", "id_name")
    selected_variables <- selected[nzchar(selected)]
    if (!all(selected_variables %in% names(reactive$data))) return()
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
      selected_variables <- unname(selected[nzchar(selected)])
      if (!all(selected_variables %in% names(reactive$data))) return()
      if (!identical(selected, reactive$model_spec)) {
        reactive$model_spec <- selected
      }
    },
    ignoreInit = TRUE
  )

  observeEvent(input$compare_competing, {
    reactive$compare_competing <- isTRUE(input$compare_competing)
  }, ignoreInit = TRUE)

  observeEvent(input$mixed_effect, {
    if (input$mixed_effect %in% c("interaction", "within")) {
      reactive$mixed_effect <- input$mixed_effect
    }
  }, ignoreInit = TRUE)

  observeEvent(list(reactive$model_spec[["within_name"]], reactive$data_version), {
    within_name <- reactive$model_spec[["within_name"]]
    if (is.null(within_name) || !nzchar(within_name)) {
      reactive$lambda_within <- NULL
      reactive$lambda_within_rival <- NULL
    } else {
      within_levels <- levels(as.factor(reactive$data[[within_name]]))
      spec <- isolate(reactive$example_spec)
      preset <- !is.null(spec) &&
        identical(within_name, unname(spec$roles[["within_name"]]))
      if (preset && !is.null(spec$within)) {
        reactive$lambda_within <- spec$within
        reactive$lambda_within_rival <- if (!is.null(spec$within_rival)) {
          spec$within_rival
        } else {
          -spec$within
        }
      } else {
        reactive$lambda_within <- create_default_lambdas(within_levels)
        reactive$lambda_within_rival <- -reactive$lambda_within
      }
    }
  }, ignoreInit = FALSE)

  output$hot_lambda_within <- rhandsontable::renderRHandsontable({
    within_name <- reactive$model_spec[["within_name"]]
    validate(need(
      within_name,
      "Select a within-subjects factor in the model table first."
    ))
    within_data <- reactive$data[[within_name]]
    lambda <- reactive$lambda_within
    competing <- isTRUE(reactive$compare_competing)
    expected_levels <- levels(as.factor(within_data))
    if (!length(lambda) || !setequal(names(lambda), expected_levels)) {
      lambda <- create_default_lambdas(expected_levels)
    }
    lambda_rival <- reactive$lambda_within_rival
    if (!length(lambda_rival) ||
        !setequal(names(lambda_rival), expected_levels)) {
      lambda_rival <- -lambda
    }
    df <- prepare_table(
      lambda, within_data,
      lambda_rival = if (competing) lambda_rival else NULL
    )
    level_width <- max(105, min(240, 20 + 6 * max(nchar(df$level))))
    weight_width <- 65
    n_width <- 45
    table_width <- level_width + weight_width + n_width + 15 +
      if (competing) weight_width else 0
    hot <- rhandsontable::rhandsontable(
      df, stretchH = "none", rowHeaders = NULL,
      width = table_width, height = min(128, 32 + 24 * nrow(df)),
      renderAllColumns = TRUE
    )
    hot <- rhandsontable::hot_col(
      hot, "level", readOnly = TRUE, width = level_width
    )
    weight_columns <- if (competing) c("favored", "rival") else "lambda"
    for (column in weight_columns) {
      hot <- rhandsontable::hot_col(hot, column, width = weight_width)
    }
    rhandsontable::hot_col(hot, "n", readOnly = TRUE, width = n_width)
  })

  observeEvent(input$hot_lambda_within, {
    changes <- input$hot_lambda_within$changes$changes
    req(length(changes) > 0)
    df <- rhandsontable::hot_to_r(input$hot_lambda_within)
    primary_column <- intersect(c("lambda", "favored"), names(df))
    if (length(primary_column)) {
      lambda <- as.numeric(df[[primary_column[[1]]]])
      names(lambda) <- as.character(df$level)
      if (!identical(lambda, isolate(reactive$lambda_within))) {
        reactive$lambda_within <- lambda
      }
    }
    if ("rival" %in% names(df)) {
      lambda_rival <- as.numeric(df$rival)
      names(lambda_rival) <- as.character(df$level)
      if (!identical(
        lambda_rival, isolate(reactive$lambda_within_rival)
      )) {
        reactive$lambda_within_rival <- lambda_rival
      }
    }
  })

  observeEvent(list(reactive$model_spec[["between_name"]], reactive$data_version), {
    between_name <- reactive$model_spec[["between_name"]]
    if (is.null(between_name) || !nzchar(between_name)) {
      reactive$lambda_between <- NULL
      reactive$lambda_between_rival <- NULL
    } else {
      between_levels <- levels(as.factor(reactive$data[[between_name]]))
      spec <- isolate(reactive$example_spec)
      preset <- !is.null(spec) &&
        identical(between_name, unname(spec$roles[["between_name"]]))
      if (preset && !is.null(spec$between)) {
        reactive$lambda_between <- spec$between
        reactive$lambda_between_rival <- if (!is.null(spec$between_rival)) {
          spec$between_rival
        } else {
          -spec$between
        }
      } else {
        reactive$lambda_between <- create_default_lambdas(between_levels)
        reactive$lambda_between_rival <- -reactive$lambda_between
      }
    }
  }, ignoreInit = FALSE)

  output$hot_lambda_between <- rhandsontable::renderRHandsontable({
    between_name <- reactive$model_spec[["between_name"]]
    validate(need(
      between_name,
      "Select a between-subjects factor in the model table first."
    ))
    between_data <- reactive$data[[between_name]]
    lambda <- reactive$lambda_between
    competing <- isTRUE(reactive$compare_competing)
    expected_levels <- levels(as.factor(between_data))
    if (!length(lambda) || !setequal(names(lambda), expected_levels)) {
      lambda <- create_default_lambdas(expected_levels)
    }
    lambda_rival <- reactive$lambda_between_rival
    if (!length(lambda_rival) ||
        !setequal(names(lambda_rival), expected_levels)) {
      lambda_rival <- -lambda
    }
    df <- prepare_table(
      lambda, between_data,
      lambda_rival = if (competing) lambda_rival else NULL
    )
    level_width <- max(105, min(240, 20 + 6 * max(nchar(df$level))))
    weight_width <- 65
    n_width <- 45
    table_width <- level_width + weight_width + n_width + 15 +
      if (competing) weight_width else 0
    hot <- rhandsontable::rhandsontable(
      df, stretchH = "none", rowHeaders = NULL,
      width = table_width, height = min(128, 32 + 24 * nrow(df)),
      renderAllColumns = TRUE
    )
    hot <- rhandsontable::hot_col(
      hot, "level", readOnly = TRUE, width = level_width
    )
    weight_columns <- if (competing) c("favored", "rival") else "lambda"
    for (column in weight_columns) {
      hot <- rhandsontable::hot_col(hot, column, width = weight_width)
    }
    rhandsontable::hot_col(hot, "n", readOnly = TRUE, width = n_width)
  })

  observeEvent(input$hot_lambda_between, {
    changes <- input$hot_lambda_between$changes$changes
    req(length(changes) > 0)
    df <- rhandsontable::hot_to_r(input$hot_lambda_between)
    primary_column <- intersect(c("lambda", "favored"), names(df))
    if (length(primary_column)) {
      lambda <- as.numeric(df[[primary_column[[1]]]])
      names(lambda) <- as.character(df$level)
      if (!identical(lambda, isolate(reactive$lambda_between))) {
        reactive$lambda_between <- lambda
      }
    }
    if ("rival" %in% names(df)) {
      lambda_rival <- as.numeric(df$rival)
      names(lambda_rival) <- as.character(df$level)
      if (!identical(
        lambda_rival, isolate(reactive$lambda_between_rival)
      )) {
        reactive$lambda_between_rival <- lambda_rival
      }
    }
  })

  selected_variable <- function(name, factor = FALSE) {
    if (is.null(name) || !nzchar(name)) return(NULL)
    value <- reactive$data[[name]]
    if (factor) as.factor(value) else value
  }

  active_lambda <- function(lambda_favored, lambda_rival, factor_label) {
    if (!isTRUE(reactive$compare_competing) || is.null(lambda_favored)) {
      return(lambda_favored)
    }
    difference <- tryCatch(
      cofad_competing_lambda(lambda_favored, lambda_rival),
      error = function(error) error
    )
    if (inherits(difference, "error")) {
      validate(need(
        FALSE, paste0(factor_label, ": ", conditionMessage(difference))
      ))
    }
    difference
  }

  analysis <- reactive({
    req(reactive$data, reactive$model_spec)
    model <- reactive$model_spec
    validate(need(
      model[["dv_name"]] %in% names(reactive$data),
      "Select a dependent variable from the current data set."
    ))
    validate(
      need(is.numeric(reactive$data[[model[["dv_name"]]]]),
           "The dependent variable must be numeric."),
      need(nzchar(model[["between_name"]]) || nzchar(model[["within_name"]]),
           "Specify at least one between- or within-subjects factor."),
      need(length(reactive$lambda_between) > 0 ||
             length(reactive$lambda_within) > 0,
           "Specify contrast weights."),
      if (nzchar(model[["within_name"]])) {
        need(nzchar(model[["id_name"]]),
             "A participant ID is required for within-subjects designs.")
      },
      if (!nzchar(model[["within_name"]]) && nzchar(model[["id_name"]])) {
        need(FALSE,
             "Select a within-subjects factor when a participant ID is used.")
      }
    )

    is_mixed <- nzchar(model[["between_name"]]) &&
      nzchar(model[["within_name"]])
    use_between_contrast <- !is_mixed ||
      !identical(reactive$mixed_effect, "within")
    lambda_between <- if (use_between_contrast) {
      active_lambda(
        reactive$lambda_between, reactive$lambda_between_rival,
        "Between-subjects competing contrasts"
      )
    } else {
      NULL
    }
    lambda_within <- active_lambda(
      reactive$lambda_within, reactive$lambda_within_rival,
      "Within-subjects competing contrasts"
    )

    calc_contrast(
      dv = reactive$data[[model[["dv_name"]]]],
      between = selected_variable(model[["between_name"]], factor = TRUE),
      lambda_between = lambda_between,
      id = selected_variable(model[["id_name"]], factor = TRUE),
      within = selected_variable(model[["within_name"]], factor = TRUE),
      lambda_within = lambda_within,
      data = NULL,
      within_score = if (is.null(input$within_score)) "L" else input$within_score
    )
  })

  output$table_region <- renderText({
    contr <- analysis()

    uses_between_contrast <- !inherits(contr, "cofad_wi")
    if (uses_between_contrast && !isTRUE(reactive$compare_competing) &&
        length(reactive$lambda_between) &&
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
    if (!isTRUE(reactive$compare_competing) &&
        length(reactive$lambda_within) &&
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

    report <- trimws(paste(utils::capture.output(print(contr)), collapse = "\n"))
    if (isTRUE(reactive$compare_competing)) {
      report <- paste(
        paste(
          "We compared two competing contrasts by z-standardizing the",
          "favored and rival weights and analyzing favored minus rival."
        ),
        report
      )
    }
    between_result <- inherits(contr, "cofad_bw") || inherits(contr, "cofad_mx")

    as.character(tagList(
      div(
        class = "cofad-results",
        tags$h4("Report"),
        tags$div(
          class = "cofad-copy-layout cofad-copy-layout-report",
          cofad_report_tag(report),
          tags$div(
            class = "cofad-copy-actions",
            tags$button(
              type = "button",
              class = "btn btn-default btn-sm cofad-copy-button",
              onclick = "cofadCopyReport(); return false;",
              "Copy report (HTML)"
            ),
            tags$span(
              id = "cofad-report-copy-status", class = "cofad-copy-status",
              role = "status"
            )
          )
        ),
        tags$textarea(
          report, id = "cofad-report-copy-text", style = "display: none;",
          `aria-hidden` = "true", tabindex = "-1"
        ),
        if (between_result) {
          tagList(
            tags$h4(if (inherits(contr, "cofad_mx")) {
              "Variance decomposition of within-contrast scores (F table)"
            } else {
              "Variance decomposition (F table)"
            }),
            tags$div(
              class = "cofad-copy-layout",
              tags$div(
                class = "cofad-table-content",
                cofad_html_table(
                  detailed_f_table(contr), id = "cofad-f-table",
                  right_align = c(
                    "SS", "df", "MS", "F", "p", "eta2", "partial_eta2"
                  )
                )
              ),
              tags$div(
                class = "cofad-copy-actions",
                cofad_f_table_export_buttons(),
                tags$span(
                  id = "cofad-f-table-copy-status",
                  class = "cofad-copy-status", role = "status"
                )
              )
            ),
            tags$p(
              class = "cofad-note",
              "The contrast is a one-degree-of-freedom component of the ",
              "overall between-group variation. F-table p values are ",
              "non-directional. ",
              shiny::HTML("<i>&eta;</i><sup>2</sup>"),
              " is the component SS divided by total SS; for the contrast ",
              "row it equals ",
              shiny::HTML("<i>r</i><sub>es</sub><sup>2</sup>"),
              ". ",
              shiny::HTML("<i>&eta;</i><sub>p</sub><sup>2</sup>"),
              " is the component SS divided by that SS plus within-group/",
              "error SS; for the contrast row it equals ",
              shiny::HTML("<i>r</i><sub>contrast</sub><sup>2</sup>"),
              ".",
              if (inherits(contr, "cofad_mx")) {
                shiny::HTML(paste0(
                  " For mixed designs, this decomposition concerns the ",
                  "participants' within-contrast ", contr$within_score,
                  if (identical(contr$within_score, "L")) {
                    " values"
                  } else {
                    " scores"
                  },
                  ", not the raw repeated outcome variance. Thus ",
                  "<i>r</i><sub>alerting</sub><sup>2</sup> ",
                  "is the share of between-group score variation matching ",
                  "the planned contrast, whereas ",
                  "<i>r</i><sub>es</sub><sup>2</sup> is its share ",
                  "of total score variation."
                ))
              }
            ),
            tags$h4(if (inherits(contr, "cofad_mx")) {
              paste(
                "Partition of total variation in within-contrast scores",
                "and alternative denominators"
              )
            } else {
              "Partition of total variation and alternative denominators"
            }),
            tags$p(
              class = "cofad-note",
              "Each row is normalized separately. Hover over a colored ",
              "component for its SS calculation."
            )
          )
        } else {
          tagList(
            tags$h4(
              "Variance decomposition of within-subjects contrast scores (F table)"
            ),
            tags$div(
              class = "cofad-copy-layout",
              tags$div(
                class = "cofad-table-content",
                cofad_html_table(
                  detailed_within_f_table(contr), id = "cofad-within-f-table",
                  right_align = c(
                    "SS", "df", "MS", "F", "p", "eta2", "partial_eta2"
                  )
                )
              ),
              tags$div(
                class = "cofad-copy-actions",
                cofad_copy_button(
                  "cofad-within-f-table", "Copy F table"
                ),
                tags$span(
                  id = "cofad-within-f-table-copy-status",
                  class = "cofad-copy-status", role = "status"
                )
              )
            ),
            tags$p(
              class = "cofad-note",
              shiny::HTML("This is the nondirectional <i>F</i>(1, "),
              unname(contr$sig[[3]]),
              shiny::HTML(") = <i>t</i><sup>2</sup> form of the same planned "),
              paste(
                "contrast. Its numerator and error mean squares are the two",
                "population-variance estimates compared by F. It is not a",
                "full repeated-measures ANOVA; the",
                "report retains the prespecified directional p value. ",
                "Within this contrast-specific decomposition, ordinary eta",
                "squared and partial eta squared are identical because total",
                "contrast-related SS is contrast SS plus its matched error SS."
              ),
              shiny::HTML(
                paste0(
                  " Thus <i>&eta;</i><sup>2</sup> = ",
                  "<i>&eta;</i><sub>p</sub><sup>2</sup> = ",
                  "<i>r</i><sub>contrast</sub><sup>2</sup>. ",
                  "The between-design <i>r</i><sub>es</sub> and ",
                  "<i>r</i><sub>alerting</sub> are not defined for this ",
                  "pure within-subjects partition."
                )
              )
            ),
            tags$h4("Partition of contrast-related variation"),
            tags$p(
              class = "cofad-note",
              "Hover over a colored component for its SS calculation."
            )
          )
        }
      )
    ))
  })

  output$variance_partition <- plotly::renderPlotly({
    contr <- analysis()
    if (inherits(contr, c("cofad_bw", "cofad_mx", "cofad_wi"))) {
      plotly_variance_partition(contr)
    }
  })

  output$r_code_region <- renderUI({
    req(reactive$data, reactive$model_spec)
    is_mixed <- nzchar(reactive$model_spec[["between_name"]]) &&
      nzchar(reactive$model_spec[["within_name"]])
    use_between_contrast <- !is_mixed ||
      !identical(reactive$mixed_effect, "within")
    code <- cofad_r_code(
      model = reactive$model_spec,
      lambda_between = if (use_between_contrast) {
        reactive$lambda_between
      } else {
        NULL
      },
      lambda_between_rival = if (use_between_contrast) {
        reactive$lambda_between_rival
      } else {
        NULL
      },
      lambda_within = reactive$lambda_within,
      lambda_within_rival = reactive$lambda_within_rival,
      compare_competing = isTRUE(reactive$compare_competing),
      within_score = if (is.null(input$within_score)) "L" else input$within_score,
      example_name = reactive$example_name
    )
    tagList(
      tags$div(
        class = "cofad-r-code-actions",
        tags$button(
          type = "button",
          class = "btn btn-default btn-sm cofad-copy-button",
          onclick = "cofadCopyRCode(); return false;",
          "Copy R code"
        ),
        tags$span(
          id = "cofad-r-code-copy-status", class = "cofad-copy-status",
          role = "status"
        )
      ),
      tags$pre(id = "cofad-r-code", class = "cofad-r-code", tags$code(code)),
      tags$textarea(
        code, id = "cofad-r-code-copy-text", style = "display: none;",
        `aria-hidden` = "true", tabindex = "-1"
      )
    )
  })

  output$citation_region <- renderUI({
    tagList(tags$hr(), cofad_citation_panel())
  })
})
