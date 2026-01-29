# Highlights specified targets and fade out the others.

Highlights specified targets and fade out the others.

## Usage

``` r
bb_proxy_focus(proxy, ids = NULL)

bb_proxy_defocus(proxy, ids = NULL)
```

## Arguments

- proxy:

  A `billboardProxy` `htmlwidget` object.

- ids:

  Data ids (names) to be highlighted, if `NULL` all datas will be
  highlighted.

## Value

A `billboardProxy` `htmlwidget` object.

## Note

`bb_defocus` is the opposite of `bb_focus`

## Examples

``` r
if (interactive()) {
library("shiny")
library("billboarder")

ui <- fluidPage(
  tags$h1("Proxy method to highlight data"),
  checkboxGroupInput(
    inputId = "focus", 
    label = "Focus", 
    choices = c("setosa", "versicolor", "virginica"), 
    inline = TRUE
  ),
  billboarderOutput(outputId = "bb")
)

server <- function(input, output, session) {
  
  output$bb <- renderBillboarder({
    billboarder() %>% 
      bb_scatterplot(
        data = iris, 
        x = "Sepal.Length", 
        y = "Sepal.Width", 
        group = "Species"
      ) %>% 
      bb_axis(x = list(tick = list(fit = FALSE))) %>% 
      bb_point(r = 8)
  })
  
  observeEvent(input$focus, {
    billboarderProxy("bb") %>% 
      bb_proxy_focus(input$focus)
  }, ignoreNULL = FALSE)
}

shinyApp(ui = ui, server = server)
}
```
