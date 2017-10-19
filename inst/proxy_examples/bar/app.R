
library("shiny")
library("billboarder")

# data ----
Titanic <- as.data.frame(Titanic)


# ui ----

ui <- fluidPage(
  
  tags$h1("Billboarder bar chart with proxy"),
  br(),
  
  fluidRow(
    
    column(
      width = 3,
      
      wellPanel(
        
        radioButtons(
          inputId = "var1", 
          label = "Choose a variable:", 
          choices = colnames(Titanic)[-5],
          selected = "Class"
        )
        
      )
      
    ),
    
    column(
      width = 9,
      billboarderOutput(outputId = "barres")
    )
    
  )
)


# server ----

server <- function(input, output, session) {
  
  output$barres <- renderBillboarder({
    dat <- aggregate(Freq ~ Class, data = Titanic, FUN = sum)
    billboarder() %>% 
      bb_barchart(data = dat)
  })
  
  observeEvent(input$var1, {
    
    dat <- aggregate(as.formula(paste("Freq", input$var1, sep = "~")), data = Titanic, FUN = sum)
    billboarderProxy(shinyId = "barres") %>% 
      bb_unload() %>%
      bb_barchart(data = dat)
    
  }, ignoreInit = TRUE)
  
  
  shiny::onStop(shiny::stopApp)
}


# run app ----

shinyApp(ui = ui, server = server)
