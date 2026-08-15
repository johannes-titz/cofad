#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar dashboardBody box
#' @importFrom shinyjs useShinyjs hidden
#' @noRd
cofad_version_value <- function() {
  version <- getOption("cofad.version")
  if (is.null(version)) {
    version <- tryCatch(
      as.character(utils::packageVersion("cofad")),
      error = function(e) "development"
    )
  }
  version
}

cofad_commit_value <- function() {
  commit <- getOption("cofad.commit")
  if (!is.null(commit) && nzchar(commit)) return(substr(commit, 1, 8))

  description <- tryCatch(
    utils::packageDescription("cofad"), error = function(e) NULL
  )
  remote_commit <- if (is.null(description)) NULL else description[["RemoteSha"]]
  if (!is.null(remote_commit) && nzchar(remote_commit)) {
    return(substr(remote_commit, 1, 8))
  }

  git_commit <- suppressWarnings(tryCatch(
    system2(
      "git", c("rev-parse", "--short=8", "HEAD"),
      stdout = TRUE, stderr = FALSE
    ),
    error = function(e) ""
  ))
  if (!length(git_commit)) return("")
  git_commit <- trimws(git_commit[[1]])
  if (nzchar(git_commit)) git_commit else ""
}

cofad_is_dev_version <- function(version = cofad_version_value()) {
  identical(version, "development") || grepl("[.]9000$", version)
}

cofad_version_label <- function(
    version = cofad_version_value(), commit = cofad_commit_value()) {
  label <- paste0("cofad v", version)
  if (cofad_is_dev_version(version)) {
    build <- if (nzchar(commit)) paste("dev", commit) else "dev"
    label <- paste0(label, " (", build, ")")
  } else if (isTRUE(getOption("cofad.webR"))) {
    build <- if (nzchar(commit)) paste("webR", commit) else "webR"
    label <- paste0(label, " (", build, ")")
  }
  label
}

cofad_version_title <- function() {
  version <- cofad_version_value()
  commit <- cofad_commit_value()
  label <- cofad_version_label(version, commit)
  is_dev <- cofad_is_dev_version(version)
  if (!is_dev && !isTRUE(getOption("cofad.webR"))) return(label)

  build <- if (is_dev) {
    if (nzchar(commit)) paste("dev", commit) else "dev"
  } else {
    paste(c(paste0("v", version), "webR", commit[nzchar(commit)]), collapse = " ")
  }
  tags$span(
    style = "white-space: nowrap;", title = label,
    tags$span(style = "font-size: 20px;", "cofad"),
    tags$span(style = "font-size: 11px; margin-left: 6px;", build)
  )
}

myui <- function(request) {
  shinyUI(
    shinydashboard::dashboardPage(
      title = "cofad: Contrast analysis",
      skin = "yellow",
      shinydashboard::dashboardHeader(title = cofad_version_title()),
      shinydashboard::dashboardSidebar(
        tags$head(
          shiny::includeScript(cofad_resource("cofad-copy.js")),
          tags$style(HTML(
            ".sidebar { padding-left: 8px; padding-right: 8px; }
             .cofad-results table { width: auto; max-width: 100%; }
             .cofad-results th { background: #f5f5f5; }
             .cofad-results td, .cofad-results th { padding: 5px 8px; }
             .cofad-results .cofad-booktabs {
               border-collapse: collapse; border-top: 2px solid #333;
               border-bottom: 2px solid #333; margin-bottom: 12px;
               background: transparent; }
             .cofad-results .cofad-booktabs thead th {
               border: 0 !important; border-bottom: 1px solid #333 !important;
               background: transparent !important; }
             .cofad-results .cofad-booktabs tbody td {
               border: 0 !important; background: transparent !important; }
             .cofad-number { text-align: right !important;
               font-variant-numeric: tabular-nums; }
             .cofad-tooltip { cursor: help; text-decoration-line: underline;
               text-decoration-style: dotted; text-decoration-thickness: 1px;
               text-underline-offset: 3px; }
             .cofad-report { white-space: pre-wrap; background: #fafafa;
               border: 1px solid #ddd; border-radius: 4px; padding: 10px;
               max-width: 820px; font-family: inherit; font-size: inherit;
               line-height: 1.45; }
             .cofad-copy-layout { display: flex; align-items: flex-start;
               gap: 8px; width: fit-content; max-width: 100%; }
             .cofad-copy-layout-report { width: 100%; }
             .cofad-copy-layout-report .cofad-report {
               flex: 1 1 520px; min-width: 0; }
             .cofad-copy-content { min-width: 0; }
             .cofad-r-code-actions { display: flex; justify-content: flex-start;
               align-items: center; gap: 8px; margin: 0 0 6px; }
             .cofad-r-code-actions .cofad-copy-button { width: auto; }
             .cofad-r-code { width: 100%; max-width: 100%; max-height: 420px;
               overflow: auto;
               white-space: pre; border: 1px solid #ddd; border-radius: 4px;
               background: #fafafa; }
             .cofad-table-content { flex: 0 1 auto; min-width: 0;
               overflow-x: auto; }
             .cofad-copy-actions { display: flex; flex: 0 0 auto;
               flex-direction: column; align-items: stretch; gap: 4px; }
             .cofad-copy-button { margin: 0; white-space: nowrap;
               width: 100%; color: #454b52 !important;
               background-color: #f5f6f7 !important;
               border: 1px solid #aeb4bb !important;
               box-shadow: 0 1px 1px rgba(0, 0, 0, .08); }
             .cofad-copy-button:hover, .cofad-copy-button:focus {
               color: #25292d !important; background-color: #e9ecef !important;
               border-color: #7f878f !important; }
             .cofad-copy-status { color: #287a31; min-height: 1.2em;
               text-align: right; }
             .cofad-hot-wrap { display: inline-block; max-width: 100%;
               overflow-x: auto; vertical-align: top; }
             .cofad-hot-wrap .rhandsontable { max-width: none; }
             .cofad-hot-wrap .ht_clone_top th { text-transform: capitalize; }
             .box-header .box-title { font-size: 18px; line-height: 1.25; }
             .box-body h4 { font-size: 15px; line-height: 1.35;
               margin-top: 14px; margin-bottom: 6px; }
             .cofad-results > h4:first-child { margin-top: 0; }
             #create_model h4 { margin-top: 8px; margin-bottom: 5px; }
             #create_model hr { margin-top: 10px; margin-bottom: 10px; }
             .cofad-note { color: #666; font-size: 90%; }
             .cofad-note-warning { color: #8a5a00; font-weight: 600; }
             .cofad-help-tooltip { cursor: help; border-bottom: 1px dotted #777; }
             #help .box-title, #help h4, #help strong { font-weight: 400; }
             .csl-entry { margin: 0 0 .8em 2em; text-indent: -2em; }
             .cofad-citation-layout .cofad-copy-content {
               flex: 1 1 360px; }
             .cofad-citation-actions .btn { margin: 0; white-space: nowrap; }
             #cofad-copy-status { color: #287a31; }
             @media (max-width: 600px) {
               .cofad-copy-layout { flex-wrap: wrap; }
               .cofad-copy-actions { margin-left: auto; }
               .cofad-table-content { flex-basis: 100%; }
             }
             .cofad-footer { color: #fff; font-size: 11px; margin-top: 24px; }"
          ))
        ),
        h4("1. Load data"),
        fileInput(
          "datafile", label = NULL,
          accept = c(
            ".csv", ".sav", "text/csv", "text/comma-separated-values",
            "application/x-spss-sav", "application/x-spss-por",
            "application/spss"
          )
        ),
        h6("Supported formats: .csv and .sav (SPSS)."),
        tags$hr(),
        cofad_example_select(),
        tags$p(
          class = "cofad-footer",
          HTML(
            "&copy; 2021&ndash;2026 Johannes Titz et al., LGPL-3.0-or-later"
          )
        )
      ),
      shinydashboard::dashboardBody(
        shinyjs::useShinyjs(),
        fluidRow(
          column(
            width = 5,
            shinyjs::hidden(
              div(
                id = "create_model",
                shinydashboard::box(
                  title = "2. Specify the model and contrasts",
                  status = "primary",
                  collapsible = TRUE,
                  width = 12,
                  uiOutput("variables")
                )
              )
            ),
            div(
              id = "help",
              shinydashboard::box(
                title = "Help and citation",
                status = "primary",
                collapsible = TRUE,
                collapsed = FALSE,
                width = 12,
                HTML(paste(
                  readLines(cofad_resource("intro.html")), collapse = ""
                )),
                tags$hr(),
                cofad_citation_panel()
              )
            )
          ),
          column(
            width = 7,
            shinyjs::hidden(
              div(
                id = "output_region",
                shinydashboard::box(
                  title = "3. Results",
                  status = "primary",
                  collapsible = TRUE,
                  width = 12,
                  htmlOutput("table_region"),
                  conditionalPanel(
                    condition = "input.between_name != '' || input.within_name != ''",
                    plotly::plotlyOutput(
                      "variance_partition", height = "300px"
                    )
                  )
                )
              )
            ),
            shinyjs::hidden(
              div(
                id = "code_region",
                shinydashboard::box(
                  title = "4. R code",
                  status = "primary",
                  collapsible = TRUE,
                  width = 12,
                  uiOutput("r_code_region")
                )
              )
            )
          )
        )
      )
    )
  )
}
