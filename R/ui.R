#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar dashboardBody box
#' @importFrom shinyjs useShinyjs hidden
#' @noRd
myui <- function(request) {
  shinyUI(
    shinydashboard::dashboardPage(
      title = "cofad: Contrast analysis",
      skin = "yellow",
      shinydashboard::dashboardHeader(title = "cofad"),
      shinydashboard::dashboardSidebar(
        tags$head(
          tags$style(HTML(
            ".sidebar { padding-left: 8px; padding-right: 8px; }
             .cofad-results table { width: 100%; }
             .cofad-results th { background: #f5f5f5; }
             .cofad-results td, .cofad-results th { padding: 5px 8px; }
             .cofad-report { white-space: pre-wrap; background: #fafafa;
               border: 1px solid #ddd; border-radius: 4px; padding: 10px; }
             .cofad-note { color: #666; font-size: 90%; }
             .cofad-note-warning { color: #8a5a00; font-weight: 600; }
             .cofad-footer { color: #999; font-size: 11px; margin-top: 24px; }"
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
        tags$p(
          class = "cofad-footer",
          HTML("cofad &copy; 2021&ndash;2026, LGPL-3.0-or-later")
        )
      ),
      shinydashboard::dashboardBody(
        fluidRow(
          shinyjs::useShinyjs(),
          div(
            id = "help",
            shinydashboard::box(
              title = "Help",
              status = "primary",
              width = 12,
              HTML(paste(
                readLines(cofad_resource("intro.html")),
                collapse = ""
              ))
            )
          ),
          shinyjs::hidden(
            div(
              id = "create_model",
              shinydashboard::box(
                title = "2. Specify the model and contrasts",
                status = "primary",
                collapsible = TRUE,
                width = 5,
                uiOutput("variables")
              )
            )
          ),
          shinyjs::hidden(
            div(
              id = "output_region",
              shinydashboard::box(
                title = "3. Results",
                status = "primary",
                width = 7,
                htmlOutput("table_region"),
                shiny::plotOutput("variance_partition", height = "250px"),
                uiOutput("citation_region")
              )
            )
          )
        )
      )
    )
  )
}
