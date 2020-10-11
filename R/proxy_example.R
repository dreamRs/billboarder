#' @title Proxy use example
#' 
#' @description Launch an example to demonstrate how to use proxy method from \code{billboarder} in Shiny app.
#'
#' @param chart Chart type for which to see an example, possible values are \code{gauge}, \code{pie}, \code{bar}, 
#' \code{bar2}, \code{line}, \code{line2}, \code{density}, \code{histogram}, \code{lollipop}, \code{stacked_bar}.
#'
#' @export
#' 
#' @importFrom shiny shinyAppDir
#'
#' @examples
#' 
#' if (interactive()) {
#' 
#' # Titanic passenger
#' proxy_example("bar")
#' 
#' # Electricity production by sources and year
#' proxy_example("bar2")
#' 
#' # Moving lollipop with mpg dataset from ggplot2
#' proxy_example("lollipop")
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
#' }
proxy_example <- function(chart = "gauge") {
  chart <- match.arg(
    arg = chart,
    choices = c("gauge", "pie", "bar", "bar2", "line", "line2",
                "density", "histogram", "stacked_bar", "lollipop"), 
    several.ok = FALSE
  )
  path <- file.path("proxy_examples", chart)
  shinyAppDir(
    appDir = system.file(path, package="billboarder", mustWork=TRUE), 
    options = list(display.mode = "showcase")
  )
}

