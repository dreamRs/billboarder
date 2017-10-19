# App update gauge ---------------------------------------------------------------

library("shiny")
library("billboarder")


# ui ----

ui <- fluidPage(
  tags$h1("Gauge with billboarder"),
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



# server ----

server <- function(input, output, session) {
  
  # define the gauge (not reactive)
  
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
  
  
  
  # update them
  
  observeEvent(input$update, {
    
    billboarderProxy("gauge1") %>% 
      bb_gaugechart(value = round(sample.int(100, 1)))
    
    billboarderProxy("gauge2") %>% 
      bb_gaugechart(value = round(sample.int(100, 1)))
    
    billboarderProxy("gauge3") %>% 
      bb_gaugechart(value = round(sample.int(100, 1)))
    
  }, ignoreInit = TRUE)
  
  shiny::onStop(shiny::stopApp)
}


# run ----

shinyApp(ui = ui, server = server)
