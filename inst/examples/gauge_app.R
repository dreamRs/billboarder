


# App update gauge ---------------------------------------------------------------



library("shiny")
library("billboarder")
library("magrittr")

ui <- fluidPage(
  tags$h1("Gauge with billboarder"),
  br(),
  fluidRow(
    column(
      width = 4,
      billboarderOutput(outputId = "gauge1")
    ),
    column(
      width = 4,
      billboarderOutput(outputId = "gauge2")
    ),
    column(
      width = 4,
      billboarderOutput(outputId = "gauge3")
    )
  ),
  actionButton(inputId = "update", label = "Update values")
)

server <- function(input, output, session) {
  
  output$gauge1 <- renderBillboarder({
    billboarder() %>% 
      bb_gaugechart(value = round(sample.int(100, 1)))
  })
  
  output$gauge2 <- renderBillboarder({
    billboarder() %>% 
      bb_gaugechart(value = round(sample.int(100, 1)))
  })
  
  output$gauge3 <- renderBillboarder({
    billboarder() %>% 
      bb_gaugechart(value = round(sample.int(100, 1)))
  })
  
  
  observeEvent(input$update, {
    
    billboarderProxy("gauge1") %>% 
      bb_gaugechart(value = round(sample.int(100, 1)))
    
    billboarderProxy("gauge2") %>% 
      bb_gaugechart(value = round(sample.int(100, 1)))
    
    billboarderProxy("gauge3") %>% 
      bb_gaugechart(value = round(sample.int(100, 1)))
    
  }, ignoreInit = TRUE)
  
}

shinyApp(ui = ui, server = server)
