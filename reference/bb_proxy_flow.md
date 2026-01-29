# Update chart flow with proxy

Update chart flow with proxy

## Usage

``` r
bb_proxy_flow(proxy, ...)
```

## Arguments

- proxy:

  A `billboardProxy` `htmlwidget` object.

- ...:

  Arguments passed to the flow API, see
  <https://naver.github.io/billboard.js/release/latest/doc/Chart.html#flow>.

## Value

A `billboardProxy` `htmlwidget` object.

## Examples

``` r
if (interactive()) {
  library(shiny)
  library(billboarder)
  
  ui <- fluidPage(
    tags$h3("Proxy flow"),
    actionButton(
      inputId = "next_data",
      label = "Add data",
      icon = icon("arrow-right")
    ),
    billboarderOutput(outputId = "chart1"),
    
    tags$h4("Real time chart"),
    billboarderOutput(outputId = "chart2")
  )
  
  server <- function(input, output, session) {
    
    
    time_data <- reactiveValues(df = data.frame(
      x = Sys.Date() + 1:20,
      y = round(rnorm(20) * 10)
    ))
    
    output$chart1 <- renderBillboarder({
      billboarder() %>% 
        bb_linechart(data = isolate(time_data$df))
    })
    
    observeEvent(input$next_data, {
      time_data$df$x <- time_data$df$x + 21
      time_data$df$y <- round(rnorm(20) * 10)
    })
    
    observe({
      billboarderProxy("chart1") %>% 
        bb_proxy_flow(json = as.list(time_data$df), duration = 1500)
    })
    
    
    
    output$chart2 <- renderBillboarder({
      df <- data.frame(
        x = Sys.time() - 1:20 * 2,
        y = round(rnorm(20) * 10)
      )
      billboarder() %>% 
        bb_linechart(data = df) %>% 
        bb_x_axis(tick = list(format = "%H:%M", fit = FALSE))
    })
    
    observe({
      invalidateLater(2000)
      billboarderProxy("chart2") %>% 
        bb_proxy_flow(json = list(
          x = list(format(Sys.time())),
          y = list(round(rnorm(1) * 10))
        ), data = list(x = "x"))
    })
    
  }
  
  shinyApp(ui, server)
}
```
