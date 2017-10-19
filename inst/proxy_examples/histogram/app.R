library("shiny")
library("billboarder")

# data ----
data("diamonds", package = "ggplot2")


# ui ----

ui <- fluidPage(
  
  tags$h1("Billboarder histogram with proxy"),
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
          inputId = "bins", 
          label = "Bins", 
          min = 5,
          max = 60, 
          value = 30, 
          step = 1
        )
        
      )
      
    ),
    
    column(
      width = 9,
      billboarderOutput(outputId = "histo")
    )
    
  )
)


# server ----

server <- function(input, output, session) {
  
  output$histo <- renderBillboarder({
    billboarder() %>% 
      bb_histogram(data = diamonds, x = isolate(input$var), bins = isolate(input$bins)) %>% 
      bb_colors_manual()
  })
  
  observeEvent(list(input$var, input$bins), {
    
    billboarderProxy(shinyId = "histo") %>% 
      bb_histogram(data = diamonds, x = input$var, bins = input$bins)
    
  }, ignoreInit = TRUE)
  
  shiny::onStop(shiny::stopApp)
}


# run app ----

shinyApp(ui = ui, server = server)
