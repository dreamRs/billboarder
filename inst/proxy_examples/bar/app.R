
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
          label = "Choose a variable", 
          choices = colnames(Titanic)[-5],
          selected = "Class"
        )
        
      )
      
    ),
    
    column(
      width = 6,
      billboarderOutput(outputId = "barres")
    )
    
  )
)


# server ----

server <- function(input, output, session) {
  
  output$barres <- renderBillboarder({
    billboarder() %>% 
      bb_barchart(data = aggregate(Freq ~ Class, data = Titanic, FUN = sum))
  })
  
  observeEvent(input$var1, {
    
    dat <- aggregate(as.formula(paste("Freq", input$var1, sep = "~")), data = Titanic, FUN = sum)
    
    billboarderProxy(shinyId = "barres") %>% 
      bb_unload() %>%
      bb_barchart(data = dat)
    
  }, ignoreInit = TRUE)
  
}


# run app ----

shinyApp(ui = ui, server = server)
