#' Start the cofad Shiny app
#'
#' @import shiny
#' @export
run_app <- function() {
  shinyApp(
    ui = myui(),
    server = myserver,
    options = list(port = 3838)
  )
}
