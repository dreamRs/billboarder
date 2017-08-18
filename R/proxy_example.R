#' @title Proxy use example
#' 
#' @description Launch an example app to demonstrate how to use proxy method from \code{billboarder}.
#'
#' @param chart Chart type for which to see an example, possible values are \code{gauge}, \code{pie}, \code{bar}, 
#' \code{bar2}, \code{line}, \code{line2}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' if (interactive()) {
#' 
#' proxy_examples("line")
#' 
#' }
#' 
#' }
proxy_example <- function(chart = "gauge") {
  chart <- match.arg(arg = chart,
                     choices = c("gauge", "pie", "bar", "bar2", "line", "line2"), 
                     several.ok = FALSE)
  if (!requireNamespace(package = "shiny"))
    message("Package 'shiny' is required to run this function")
  path <- file.path("proxy_examples", chart)
  shiny::runApp(
    appDir = system.file(path, package="billboarder", mustWork=TRUE), 
    display.mode = "showcase"
  )
}



