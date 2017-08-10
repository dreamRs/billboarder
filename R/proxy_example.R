#' @title Proxy use example
#' 
#' @description Launch an example app to demonstrate how to use proxy method from \code{billboarder}.
#'
#' @param chart Chart type for which to see an example.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' if (interactive()) {
#' 
#' proxy_examples("gauge")
#' 
#' }
#' 
#' }
proxy_example <- function(chart = c("gauge", "pie", "bar", "line")) {
  chart <- match.arg(arg = chart, several.ok = FALSE)
  if (!requireNamespace(package = "shiny"))
    message("Package 'shiny' is required to run this function")
  path <- paste("proxy_examples", chart, sep = "/")
  shiny::runApp(
    appDir = system.file(path, package="billboarder", mustWork=TRUE), 
    display.mode = "showcase"
  )
}



