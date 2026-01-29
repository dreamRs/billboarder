# Shiny bindings for billboarder

Output and render functions for using billboarder within Shiny
applications and interactive Rmd documents.

## Usage

``` r
billboarderOutput(outputId, width = "100%", height = "400px")

renderBillboarder(expr, env = parent.frame(), quoted = FALSE)

billboarderProxy(
  shinyId,
  data = NULL,
  session = shiny::getDefaultReactiveDomain()
)
```

## Arguments

- outputId:

  output variable to read from

- width, height:

  Must be a valid CSS unit (like `'100%'`, `'400px'`, `'auto'`) or a
  number, which will be coerced to a string and have `'px'` appended.

- expr:

  An expression that generates a billboarder

- env:

  The environment in which to evaluate `expr`.

- quoted:

  Is `expr` a quoted expression (with
  [`quote()`](https://rdrr.io/r/base/substitute.html))? This is useful
  if you want to save an expression in a variable.

- shinyId:

  single-element character vector indicating the output ID of the chart
  to modify (if invoked from a Shiny module, the namespace will be added
  automatically)

- data:

  A `data.frame`.

- session:

  the Shiny session object to which the chart belongs; usually the
  default value will suffice

## See also

[`proxy_example`](https://dreamrs.github.io/billboarder/reference/proxy_example.md)

## Examples

``` r
if (interactive()) {
  library(shiny)
  
  ui <- fluidPage(
    tags$h2("Include billboard charts in Shiny"),
    fluidRow(
      column(
        width = 6,
        billboarderOutput("mybb1"),
        tags$p("Click on a bar to get the value:"),
        verbatimTextOutput("res_click")
      ),
      column(
        width = 6,
        billboarderOutput("mybb2")
      )
    )
  )
  
  server <- function(input, output, session) {
    
    output$mybb1 <- renderBillboarder({
      
      dat <- data.frame(
        label = paste("Label", 1:5),
        value = sample.int(100, 5)
      )
      
      billboarder() %>%
        bb_barchart(
          data = dat,
          mapping = bbaes(label, value),
          rotated = TRUE
        )
    })
    
    output$res_click <- renderPrint({
      input$mybb1_click
    })
    
    
    output$mybb2 <- renderBillboarder({
      
      data(AirPassengers)
      
      air_passengers <- data.frame(
        date = as.Date(paste(
          rep(1949:1960, each = 12),
          rep(1:12, times = 12),
          "01", sep = "-"
        )), 
        passengers = AirPassengers
      )
      
      billboarder() %>% 
        bb_linechart(
          data = air_passengers, 
          mapping = bbaes(date, passengers), type = "spline"
        ) %>% 
        bb_x_axis(tick = list(format = "%Y", fit = FALSE))
    })
    
  }
  
  shinyApp(ui, server)
}
```
