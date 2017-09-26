
library("shiny")
library("billboarder")

ui <- fluidPage(
  tags$h1("Transform chart type with proxy"),
  
  fluidRow(
    column(
      width = 3,
      radioButtons(
        inputId = "type",
        label = "Chart type:",
        choices = c(
          "line",
          "spline",
          "step",
          "area",
          "area-spline",
          "area-step",
          "bar",
          "scatter",
          "pie",
          "donut",
          "gauge"
        ),
        selected = "line"
      ),
      tags$p("'pie', 'donut' require to modify data.",
             " And it's not very relevant to transform those data to 'gauge'.")
    ),
    column(
      width = 9,
      billboarderOutput(outputId = "mychart")
    )
  )
)

server <- function(input, output, session) {
  
  output$mychart <- renderBillboarder({
    billboarder() %>% 
      bb_linechart(data = data.frame(month = month.name, AirPassengers = tail(AirPassengers, 12)), 
                   x = "month") %>% 
      bb_x_axis(type = "category")
  })
  
  
  observe({
    billboarderProxy(shinyId = "mychart") %>% 
      bb_proxy_transform(type = input$type)
  })
  
}

shinyApp(ui = ui, server = server)
