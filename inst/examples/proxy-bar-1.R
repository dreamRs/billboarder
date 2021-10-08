
library("shiny")
library("billboarder")
library("magrittr")

ui <- fluidPage(
  tags$h1("Update billboard chart"),
  
  billboarderOutput(outputId = "mybb1"),
  
  actionButton(inputId = "update", label = "Update data !")
)

server <- function(input, output, session) {
  
  data_r <- reactive({
    input$update
    data.frame(x = letters[1:10], y = round(runif(n = 10, min = 10, max = 100)))
  })
  
  output$mybb1 <- renderBillboarder({
    billboarder() %>%
      bb_barchart(data = isolate(data_r())) %>%
      bb_axis(y = list(max = 100, padding = 0, tick = list(outer = FALSE)))
  })
  
  observeEvent(input$update, {
    billboarderProxy("mybb1") %>% 
      bb_barchart(data = data_r())
  }, ignoreInit = TRUE)
}

shinyApp(ui = ui, server = server)
