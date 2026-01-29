# Change names of the data with proxy

Change names of the data with proxy

## Usage

``` r
bb_proxy_data_names(proxy, old = NULL, new = NULL)
```

## Arguments

- proxy:

  A `billboardProxy` `htmlwidget` object.

- old:

  Old names

- new:

  New names

## Value

A `billboardProxy` `htmlwidget` object.

## Examples

``` r
if (interactive()) {

library(shiny)
library(billboarder)

ui <- fluidPage(
  tags$h2("Update axis title & data name (tooltip & legend)"),
  billboarderOutput(outputId = "my_bb"),
  textInput(
    inputId = "new_name",
    label = "New name :",
    value = "this is a new name",
    width = "100%"
  ),
  actionButton(
    inputId = "update",
    label = "Update chart",
    width = "100%"
  )
)

server <- function(input, output, session) {
  
  output$my_bb <- renderBillboarder({
    dat <- sample(letters[1:5], 100, TRUE)
    billboarder() %>%
      bb_barchart(data = table(dat)) %>% 
      bb_y_axis(label = list(text = "Freq"))
  })
  
  observeEvent(input$update, {
    dat <- sample(letters[1:5], 100, TRUE)
    billboarderProxy(shinyId = "my_bb") %>% 
      bb_proxy_axis_labels(y = input$new_name) %>% 
      bb_proxy_data_names(old = "Freq", 
                          new = input$new_name) %>% 
      bb_barchart(data = table(dat))
  }, ignoreInit = TRUE)
  
}

shinyApp(ui, server)

}
```
