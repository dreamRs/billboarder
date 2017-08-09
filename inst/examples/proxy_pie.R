
library("shiny")
library("billboarder")

Titanic <- as.data.frame(Titanic)

ui <- fluidPage(
  
  tags$h1("Billboarder pie chart with proxy"),
  br(),
  
  fluidRow(
    column(
      width = 3,
      
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
    ),
    column(
      width = 4,
      billboarderOutput(outputId = "camembert")
    )
  )
)

server <- function(input, output, session) {
  
  output$camembert <- renderBillboarder({
    billboarder() %>% 
      bb_piechart(data = aggregate(Freq ~ Survived, data = Titanic, FUN = sum)) %>% 
      bb_title(text = "Survival of passengers on the Titanic")
  })
  
  observeEvent(list(input$Class, input$Sex, input$Age), {
    
    ind <- with(Titanic, Class %in% input$Class & Sex %in% input$Sex & Age %in% input$Age)
    
    Titanic_f <- Titanic[ind, ]
    
    billboarderProxy(shinyId = "camembert") %>% 
      bb_piechart(data = aggregate(Freq ~ Survived, data = Titanic_f, FUN = sum))
    
  }, ignoreInit = TRUE)
}

shinyApp(ui = ui, server = server)
