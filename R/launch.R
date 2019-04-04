#' Launch
#' @export
launch_app <- function(x, ...)
{
  shiny::runApp(appDir = system.file("app", package = "chinookRoutingApp"),
                ...)
}
