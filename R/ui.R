options(shiny.autoreload = F) # for faster testing
options(shiny.sanitize.errors = FALSE) # (handle errors manually)
enableBookmarking("url") # not currently supported, but maybe later

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

#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar dashboardBody box
#' @importFrom shinyalert useShinyalert
#' @importFrom shinyjs useShinyjs hidden
#' @importFrom shinyBS bsTooltip
#' @importFrom shinybusy add_busy_spinner
#' @noRd
myui <- function(request){
shinyUI(
  shinydashboard::dashboardPage(
    title = "cofad-app",
    skin = "blue",
    shinydashboard::dashboardHeader(title = "cofad-app"),
      # Sidebar-----------------------------------------------------------------
      shinydashboard::dashboardSidebar(
        shinybusy::add_busy_spinner(spin = "self-building-square",
                                    position = "bottom-right",
                                    margin = c(50, 0)),
        tags$head(tags$style(HTML('.sidebar {padding-left: 8px;}'))),
        # load data
        h4("1. Load data"),
        uiOutput("file_area"),
        h6("Currently, you can only load .csv files and .sav (SPSS) files."),
        HTML('<footer><font size="1"><p style="color:grey">cofad-app &copy; 2021 Johannes Titz & Markus Burkhardt, license AGPL</p></font></footer>')
      ),
     shinydashboard::dashboardBody(
              tags$script(HTML("$(document).on('shiny:sessioninitialized', function(event) {
  var isSafari = navigator.vendor && navigator.vendor.indexOf('Apple') > -1 &&
               navigator.userAgent &&
               navigator.userAgent.indexOf('CriOS') == -1 &&
               navigator.userAgent.indexOf('FxiOS') == -1;
  Shiny.onInputChange('isSafari', isSafari);
});")),

       #includeScript("checkbrowser.js"),
       shinyalert::useShinyalert(), # for manual error handling, has to be in dashboardBody
        # Model spec and model display -----------------------------------------
        fluidRow(
          shinyjs::useShinyjs(),
          div(id = "help",
              shinydashboard::box(title = "Help", status = "primary",
                      HTML('<p>How to use cofad? See <a href="https://github.com/johannes-titz/cofad/blob/master/README.md" target="_blank">README</a> for a short introduction.</p>
                  <p>Bugtracker: <a href="https://github.com/johannes-titz/cofad/issues" target="_blank">https://github.com/johannes-titz/cofad/issues</a></p>'))),
          shinyjs::hidden(div(id = "create_model",
          shinydashboard::box(title = "2. Create model", status = "primary", collapsible = T,
              width = 6,
              uiOutput("variables"),

          ),
    #       fluidRow(
    #   class = "panel-body",
    # )
    )),
          shinyjs::hidden(div(id = "display_model",
            shinydashboard::box(title = "Model", status = "primary", collapsible = T, width = 4,
                # level 1
                strong("Level 1"),
                br(),
                uiOutput("mod_l1"),
                # level 2
                br(), strong("Level 2"),
                uiOutput("mod_l2"),
                # model formula
                br(), strong("R model formula"),
                uiOutput("mod_r")
            ))
          ),
        # Output Table, Download -----------------------------------------------
       column(width = 6,
         shinyjs::hidden(div(id = "output_region",
                    shinydashboard::box(title = "3. Result",
                        status = "primary",
                        width = 6,
                        uiOutput("table_region"))#,
                        #br(),
                        #downloadButton("download", "Download Table")),

                    )
                )
         ))


  )
  )
)
}
