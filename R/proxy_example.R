#' @title Proxy use example
#' 
#' @description Launch an example to demonstrate how to use proxy method from \code{billboarder} in Shiny app.
#'
#' @param chart Chart type for which to see an example, possible values are \code{gauge}, \code{pie}, \code{bar}, 
#' \code{bar2}, \code{line}, \code{line2}, \code{density}, \code{histogram}, \code{stacked_bar} or \code{transform} (for changing type of chart).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' if (interactive()) {
#' 
#' # Titanic passenger
#' proxy_example("bar")
#' 
#' # Electricity production by sources and year
#' proxy_example("bar2")
#' 
#' # Update a stacked bar chart
#' proxy_example("stacked_bar")
#' 
#' # Moving sine and cosine
#' proxy_example("line")
#' 
#' # Changing lines and adding ones
#' proxy_example("line2")
#' 
#' # Update pie chart
#' proxy_example("pie")
#' 
#' # Density with ggplot2 diamonds
#' proxy_example("density")
#' 
#' # Histogram with ggplot2 diamonds
#' proxy_example("histogram")
#' 
#' # Update chart type
#' proxy_example("transform")
#' 
#' }
#' 
#' }
proxy_example <- function(chart = "gauge") {
  chart <- match.arg(
    arg = chart,
    choices = c("gauge", "pie", "bar", "bar2", "line", "line2", "density", "histogram", "transform", "stacked_bar"), 
    several.ok = FALSE
  )
  if (!requireNamespace(package = "shiny"))
    message("Package 'shiny' is required to run this function")
  path <- file.path("proxy_examples", chart)
  shiny::runApp(
    appDir = system.file(path, package="billboarder", mustWork=TRUE), 
    display.mode = "showcase"
  )
}



