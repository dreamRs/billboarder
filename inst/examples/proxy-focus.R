
library("shiny")
library("billboarder")
library("magrittr")

ui <- fluidPage(
  tags$h1("Proxy method to highlight data"),
  checkboxGroupInput(
    inputId = "focus", 
    label = "Focus", 
    choices = c("setosa", "versicolor", "virginica"), 
    inline = TRUE
  ),
  billboarderOutput(outputId = "bb")
)

server <- function(input, output, session) {
  
  output$bb <- renderBillboarder({
    billboarder() %>% 
      bb_scatterplot(
        data = iris, 
        x = "Sepal.Length", 
        y = "Sepal.Width", 
        group = "Species"
      ) %>% 
      bb_axis(x = list(tick = list(fit = FALSE))) %>% 
      bb_point(r = 8)
  })
  
  observeEvent(input$focus, {
    billboarderProxy("bb") %>% 
      bb_proxy_focus(input$focus)
  }, ignoreNULL = FALSE)
}

shinyApp(ui = ui, server = server)