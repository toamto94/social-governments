#' @export

run_cicero <- function() {
    app <- shiny::shinyApp(ui = ui, server = server)
    shiny::runApp(app)
}
