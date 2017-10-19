library("shiny")
library("billboarder")

# data ----
data("diamonds", package = "ggplot2")


# ui ----

ui <- fluidPage(
  
  tags$h1("Billboarder density plot with proxy"),
  br(),
  
  fluidRow(
    
    column(
      width = 3,
      
      wellPanel(
        
        radioButtons(
          inputId = "var", 
          label = "Variable:", 
          choices = c("carat", "depth", "table", "price")
        ),
        
        sliderInput(
          inputId = "bandwidth", 
          label = "Bandwidth", 
          min = 0.1,
          max = 5, 
          value = 1, 
          step = 0.1
        )
        
      )
      
    ),
    
    column(
      width = 9,
      billboarderOutput(outputId = "density")
    )
    
  )
)


# server ----

server <- function(input, output, session) {
  
  output$density <- renderBillboarder({
    billboarder() %>% 
      bb_densityplot(data = diamonds, x = isolate(input$var), adjust = isolate(input$bandwidth))
  })
  
  observeEvent(list(input$var, input$bandwidth), {
    
    billboarderProxy(shinyId = "density") %>% 
      bb_densityplot(data = diamonds, x = input$var, adjust = input$bandwidth)
    
  }, ignoreInit = TRUE)
  
  
  shiny::onStop(shiny::stopApp)
}


# run app ----

shinyApp(ui = ui, server = server)
