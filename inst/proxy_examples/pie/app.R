
library("shiny")
library("billboarder")

# data ----
Titanic <- as.data.frame(Titanic)


# ui ----

ui <- fluidPage(
  
  tags$h1("Billboarder pie chart with proxy"),
  br(),
  
  fluidRow(
    
    column(
      width = 4,
      
      wellPanel(
        tags$h3("Filter passengers :"),
        
        checkboxGroupInput(
          inputId = "Class", label = "Class", inline = TRUE, 
          choices = unique(Titanic$Class), selected = unique(Titanic$Class)
        ),
        checkboxGroupInput(
          inputId = "Sex", label = "Sex", inline = TRUE, 
          choices = unique(Titanic$Sex), selected = unique(Titanic$Sex)
        ),
        checkboxGroupInput(
          inputId = "Age", label = "Age", inline = TRUE, 
          choices = unique(Titanic$Age), selected = unique(Titanic$Age)
        )
      )
      
    ),
    
    column(
      width = 4,
      billboarderOutput(outputId = "camembert")
    )
    
  )
)


# server ----

server <- function(input, output, session) {
  
  # initialize chart
  
  output$camembert <- renderBillboarder({
    billboarder() %>% 
      bb_piechart(data = aggregate(Freq ~ Survived, data = Titanic, FUN = sum)) %>% 
      bb_title(text = "Survival of passengers on the Titanic")
  })
  
  
  # update on filters
  
  observeEvent(list(input$Class, input$Sex, input$Age), {
    
    ind <- with(Titanic, Class %in% input$Class & Sex %in% input$Sex & Age %in% input$Age)
    
    Titanic_f <- Titanic[ind, ]
    
    billboarderProxy(shinyId = "camembert") %>% 
      bb_piechart(data = aggregate(Freq ~ Survived, data = Titanic_f, FUN = sum))
    
  }, ignoreInit = TRUE)
  
  shiny::onStop(shiny::stopApp)
}


# run app ----

shinyApp(ui = ui, server = server)
