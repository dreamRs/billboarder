
#' @title Simple Gauge
#' 
#' @description A gauge that automatically updates itself in Shiny apps.
#'
#' @param value Value for the gauge.
#' @param min Minimal value for the gauge, default to \code{0}.
#' @param max Maximal value for the gauge, default to \code{100}.
#' @param colors Vector of color(s), if more than one, \code{steps} must be specified.
#' @param steps Upper bound for changing colors.
#' @param label_tooltip Label to appear on the tooltip, when mouse is hovering the gauge.
#' @param label_show Show or not minimal and maximal labels.
#' @param label_format JavaScript function to format inner label.
#' @param label_extents JavaScript function to set custom labels.
#' @param expand Enable or disable expanding gauge.
#' @param subtitle Additional text to add below the value.
#' @param full_circle Show full circle as donut. When set to \code{TRUE},
#'  the max label will not be showed due to start and end points are same location.
#' @param gauge_width Set width of gauge chart.
#' @param width Width of the element container.
#' @param height Height of the element container.
#' @param elementId Use an explicit element ID for the widget.
#'
#' @export
#' 
#' @importFrom htmlwidgets createWidget
#'
#' @examples
#' 
#' bauge(45)
#' 
#' bauge(67, colors = "#F6C600")
#' 
#' bauge(90, full_circle = TRUE)
#' 
#' bauge(90, max = 210, gauge_width = 20, label_format = suffix(" km/h"))
#' 
#' # Shiny example
#' if (interactive()) {
#'   library(shiny)
#'   
#'   ui <- fluidPage(
#'     baugeOutput(outputId = "gauge", width = "300px"),
#'     actionButton(inputId = "update_value", label = "Update value"),
#'     actionButton(inputId = "update_max", label = "Update max")
#'   )
#'   
#'   server <- function(input, output, session) {
#'     
#'     value <- reactive({
#'       input$update_value
#'       round(sample.int(100, 1))
#'     })
#'     
#'     max_value <- reactive({
#'       input$update_max
#'       sample(100:200, 1)
#'     })
#'     
#'     output$gauge <- renderBauge({
#'       bauge(
#'         value = value(),
#'         max = max_value(),
#'         steps = c(30, 60, 90, 100),
#'         colors = c("#FF0000", "#F97600", "#F6C600", "#60B044")
#'       )
#'     })
#'     
#'   }
#'   
#'   shinyApp(ui, server)
#' }
bauge <- function(value,
                  min = 0, 
                  max = 100, 
                  colors = NULL, 
                  steps = NULL, 
                  label_tooltip = NULL,
                  label_show = TRUE, 
                  label_format = NULL,
                  label_extents = NULL, 
                  expand = TRUE,
                  subtitle = NULL, 
                  full_circle = FALSE,
                  gauge_width = NULL,
                  width = NULL, 
                  height = NULL,
                  elementId = NULL) {

  if (!isTRUE(nzchar(label_tooltip)))
    label_tooltip <- "value:"
  
  x = dropNulls(list(
    data = list(
      json = setNames(list(list(value)), label_tooltip),
      type = "gauge"
    ),
    gauge = dropNulls(list(
      min = min, max = max,
      units = subtitle,
      fullCircle = full_circle,
      expand = expand,
      width = gauge_width,
      label = dropNulls(list(
        show = label_show, 
        format = label_format,
        extents = label_extents
      ))
    )),
    legend = list(show = FALSE),
    color = dropNulls(list(
      pattern = list1(colors),
      threshold = dropNulls(list(values = steps))
    ))
  ))

  htmlwidgets::createWidget(
    name = "bauge",
    x = x,
    width = width,
    height = height,
    package = "billboarder",
    elementId = elementId,
    dependencies = billboard_dependencies(),
    sizingPolicy = sizingPolicy(
      defaultWidth = "100%",
      defaultHeight = "100%",
      viewer.defaultHeight = "100%",
      viewer.defaultWidth = "100%",
      browser.defaultHeight = "100%",
      browser.defaultWidth = "100%",
      knitr.defaultHeight = "320px",
      knitr.figure = FALSE,
      browser.fill = TRUE,
      padding = 10
    )
  )
}


#' Shiny bindings for bauge
#'
#' Output and render functions for using bauge within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a bauge
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name bauge-shiny
#'
#' @export
baugeOutput <- function(outputId, width = "100%", height = "200px"){
  htmlwidgets::shinyWidgetOutput(outputId, "bauge", width, height, package = "billboarder")
}

#' @rdname bauge-shiny
#' @export
renderBauge <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, baugeOutput, env, quoted = TRUE)
}
