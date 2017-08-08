


# Click in Shiny ----------------------------------------------------------


library("shiny")
library("billboarder")
library("magrittr")
library("data.table")


data("mpg", package = "ggplot2")
setDT(mpg)

stars <- data.frame(
  package = c("billboarder", "ggiraph", "officer", "shinyWidgets", "visNetwork"),
  stars = c(9, 177, 43, 44, 169)
)


ui <- fluidPage(
  tags$h1("Shiny integration"),
  
  billboarderOutput(outputId = "mybb1"),
  
  fluidRow(
    column(
      width = 6,
      verbatimTextOutput(outputId = "click1")
    ),
    column(
      width = 6,
      verbatimTextOutput(outputId = "over1")
    )
  ),
  
  billboarderOutput(outputId = "mybb2"),
  
  verbatimTextOutput(outputId = "res2")
)

server <- function(input, output, session) {
  
  output$mybb1 <- renderBillboarder({
    billboarder() %>%
      bb_barchart(data = mpg[, list(count = .N), by = manufacturer][order(count)]) %>%
      # bb_data(selection = list(enabled = TRUE, multiple = FALSE)) %>% 
      bb_axis(rotated = TRUE) %>%
      bb_title(text = "Number of models by manufacturer", position = "left-top")
  })
  
  output$click1 <- renderPrint({
    input$mybb1_click
  })
  output$over1 <- renderPrint({
    input$mybb1_over
  })
  
  
  output$mybb2 <- renderBillboarder({
    billboarder() %>% 
      bb_piechart(data = stars)
  })
  
  output$res2 <- renderPrint({
    input$mybb2_click
  })
  
}

shinyApp(ui = ui, server = server)
