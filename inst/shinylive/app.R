suppressPackageStartupMessages({
  library(shiny)
  library(plotly)
  library(rhandsontable)
  library(shinydashboard)
  library(shinyjs)
})

source("build-info.R", local = TRUE)
options(
  cofad.resource_dir = "extdata",
  cofad.data_dir = "data"
)

source("R/calc_contrast.R", local = TRUE)
source("R/calc_contrast_aggregated.R", local = TRUE)
source("R/design_detection.R", local = TRUE)
source("R/examples.R", local = TRUE)
source("R/helper.R", local = TRUE)
source("R/print_methods.R", local = TRUE)
source("R/summary_methods.R", local = TRUE)
source("R/ui.R", local = TRUE)
source("R/server.R", local = TRUE)

shinyApp(ui = myui(), server = myserver)
