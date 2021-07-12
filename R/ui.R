options(shiny.sanitize.errors = FALSE) # (handle errors manually)
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar
#'   dashboardBody box
#' @importFrom shinyalert useShinyalert
#' @importFrom shinyjs useShinyjs hidden
#' @importFrom shinyBS bsTooltip
#' @importFrom shinybusy add_busy_spinner
#' @noRd
myui <- function(request) {
  shinyUI(
    shinydashboard::dashboardPage(
      title = "cofad-app",
      skin = "yellow",
      shinydashboard::dashboardHeader(
        title = "cofad-app"),
      # Sidebar-----------------------------------------------------------------
      shinydashboard::dashboardSidebar(
        shinybusy::add_busy_spinner(
          spin = "self-building-square",
          position = "bottom-right",
          margin = c(50, 0)
        ),
        tags$head(tags$style(HTML(".sidebar {padding-left: 8px;}"))),
        # load data
        h4("1. Load data"),
        uiOutput("file_area"),
        h6("Currently, you can only load .csv files and .sav (SPSS) files."),
        HTML(
          paste(
            "<footer><font size='1'><p style='color:grey'>cofad-app &copy;",
            "2021 Johannes Titz & Markus Burkhardt, license AGPL",
            "</p></font></footer>",
            sep = ""
          )
        )
      ),
      shinydashboard::dashboardBody(
        tags$script(
          HTML("$(document).on('shiny:sessioninitialized', function(event) {
  var isSafari = navigator.vendor && navigator.vendor.indexOf('Apple') > -1 &&
               navigator.userAgent &&
               navigator.userAgent.indexOf('CriOS') == -1 &&
               navigator.userAgent.indexOf('FxiOS') == -1;
  Shiny.onInputChange('isSafari', isSafari);
});")),
        # for manual error handling, has to be in dashboardBody
        shinyalert::useShinyalert(),
        # Model spec and model display -----------------------------------------
        fluidRow(
          shinyjs::useShinyjs(),
          div(
            id = "help",
            shinydashboard::box(
              title = "Help",
              status = "primary",
              HTML(
                paste(
                  "<p>How to use cofad? See <a href='https://github.com/",
                  "johannes-titz/cofad#readme' target='_blank'>",
                  "README</a> for a short introduction.</p>",
                  "<p>Bugtracker: <a href='https://github.com/johannes-titz/",
                  "cofad/issues' target='_blank'>https://github.com/",
                  "johannes-titz/cofad/issues</a></p>",
                  "<p>Example data set (Table 5.3 from Rosenthal and Rosnow",
                  ", 2000): <a href='https://cofad.titz.science/example' ",
                  "target='_blank'>https://cofad.titz.science/example</a>",
                  "<p>Citation: [coming soon]</p>",
                  sep = ""
                )
              )
            )
          ),
          shinyjs::hidden(
            div(
              id = "create_model",
              shinydashboard::box(
                title = "2. Create model",
                status = "primary", collapsible = T,
                width = 6,
                uiOutput("variables"),

              ),
            )
          ),
          # Output  ------------------------------------------------------------
          column(
            width = 6,
            shinyjs::hidden(
              div(
                id = "output_region",
                shinydashboard::box(
                  title = "3. Result",
                  status = "primary",
                  width = 6,
                  uiOutput("table_region")
                )
              )
            )
          )
        )
      )
    )
  )
}
