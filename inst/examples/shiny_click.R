


# Click in Shiny ----------------------------------------------------------


library("shiny")
library("billboarder")
library("magrittr")



ui <- fluidPage(
  tags$h1("Update billboard chart"),
  
  billboarderOutput(outputId = "mybb1"),
  
  verbatimTextOutput(outputId = "res")
)

server <- function(input, output, session) {
  
  output$mybb1 <- renderBillboarder({
    billboarder() %>%
      bb_bar(data = mpg[, list(count = .N), by = manufacturer][order(count)]) %>%
      bb_axis(rotated = TRUE) %>%
      bb_title(text = "Number of models by manufacturer", position = "left-top") %>% 
      bb_click(inputId = "click")
  })
  
  output$res <- renderPrint({
    input$click
  })
  
}

shinyApp(ui = ui, server = server)
