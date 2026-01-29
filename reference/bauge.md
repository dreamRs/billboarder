# Simple Gauge

A gauge that automatically updates itself in Shiny apps.

## Usage

``` r
bauge(
  value,
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
  elementId = NULL
)
```

## Arguments

- value:

  Value for the gauge.

- min:

  Minimal value for the gauge, default to `0`.

- max:

  Maximal value for the gauge, default to `100`.

- colors:

  Vector of color(s), if more than one, `steps` must be specified.

- steps:

  Upper bound for changing colors.

- label_tooltip:

  Label to appear on the tooltip, when mouse is hovering the gauge.

- label_show:

  Show or not minimal and maximal labels.

- label_format:

  JavaScript function to format inner label.

- label_extents:

  JavaScript function to set custom labels.

- expand:

  Enable or disable expanding gauge.

- subtitle:

  Additional text to add below the value.

- full_circle:

  Show full circle as donut. When set to `TRUE`, the max label will not
  be showed due to start and end points are same location.

- gauge_width:

  Set width of gauge chart.

- width:

  Width of the element container.

- height:

  Height of the element container.

- elementId:

  Use an explicit element ID for the widget.

## Examples

``` r
bauge(45)

{"x":{"data":{"json":{"value:":[45]},"type":"gauge"},"gauge":{"min":0,"max":100,"fullCircle":false,"expand":true,"label":{"show":true}},"legend":{"show":false},"color":{"threshold":{}}},"evals":[],"jsHooks":[]}
bauge(67, colors = "#F6C600")

{"x":{"data":{"json":{"value:":[67]},"type":"gauge"},"gauge":{"min":0,"max":100,"fullCircle":false,"expand":true,"label":{"show":true}},"legend":{"show":false},"color":{"pattern":["#F6C600"],"threshold":{}}},"evals":[],"jsHooks":[]}
bauge(90, full_circle = TRUE)

{"x":{"data":{"json":{"value:":[90]},"type":"gauge"},"gauge":{"min":0,"max":100,"fullCircle":true,"expand":true,"label":{"show":true}},"legend":{"show":false},"color":{"threshold":{}}},"evals":[],"jsHooks":[]}
bauge(90, max = 210, gauge_width = 20, label_format = suffix(" km/h"))

{"x":{"data":{"json":{"value:":[90]},"type":"gauge"},"gauge":{"min":0,"max":210,"fullCircle":false,"expand":true,"width":20,"label":{"show":true,"format":"function(x) {return x + ' km/h';}"}},"legend":{"show":false},"color":{"threshold":{}}},"evals":["gauge.label.format"],"jsHooks":[]}
# Shiny example
if (interactive()) {
  library(shiny)
  
  ui <- fluidPage(
    baugeOutput(outputId = "gauge", width = "300px"),
    actionButton(inputId = "update_value", label = "Update value"),
    actionButton(inputId = "update_max", label = "Update max")
  )
  
  server <- function(input, output, session) {
    
    value <- reactive({
      input$update_value
      round(sample.int(100, 1))
    })
    
    max_value <- reactive({
      input$update_max
      sample(100:200, 1)
    })
    
    output$gauge <- renderBauge({
      bauge(
        value = value(),
        max = max_value(),
        steps = c(30, 60, 90, 100),
        colors = c("#FF0000", "#F97600", "#F6C600", "#60B044")
      )
    })
    
  }
  
  shinyApp(ui, server)
}
```
