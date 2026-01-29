# Change colors with proxy

Change colors with proxy

## Usage

``` r
bb_proxy_data_colors(proxy, names = NULL, colors = NULL)
```

## Arguments

- proxy:

  A `billboardProxy` `htmlwidget` object.

- names:

  Names of series

- colors:

  New colors, in same order that `names`.

## Value

A `billboardProxy` `htmlwidget` object.

## Examples

``` r
if (interactive()) {

library(shiny)
library(billboarder)

ui <- fluidPage(
  tags$h2("Update colors"),
  fluidRow(
    column(
      width = 3,
      selectizeInput(
        inputId = "col_eol",
        label = "Color for 'prod_eolien':",
        choices = c("#66C2A5", "#FC8D62", 
                    "#8DA0CB", "#E78AC3",
                    "#A6D854", "#FFD92F", 
                    "#E5C494", "#B3B3B3")
      ),
      selectizeInput(
        inputId = "col_sol",
        label = "Color for 'prod_solaire':",
        choices = c("#66C2A5", "#FC8D62", 
                    "#8DA0CB", "#E78AC3",
                    "#A6D854", "#FFD92F", 
                    "#E5C494", "#B3B3B3")
      )
    ),
    column(
      width = 9,
      billboarderOutput(outputId = "my_bb")
    )
  )
)

server <- function(input, output, session) {

  output$my_bb <- renderBillboarder({
    data(prod_par_filiere)
    billboarder() %>%
      bb_barchart(
        data = prod_par_filiere[, c(1, 6, 8)]
      )
  })

  observe({
    billboarderProxy(shinyId = "my_bb") %>%
      bb_proxy_data_colors(
        names = c("prod_eolien", "prod_solaire"),
        colors = c(input$col_eol, input$col_sol)
      )
  })

}

shinyApp(ui, server)

}
```
