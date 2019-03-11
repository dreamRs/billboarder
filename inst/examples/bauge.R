
# App update gauge ---------------------------------------------------------------

library("shiny")
library("billboarder")


# ui ----

ui <- fluidPage(
  tags$h1("Gauge with billboarder"),
  fluidRow(
    column(
      width = 4,
      baugeOutput(outputId = "gauge1"),
      baugeOutput(outputId = "gauge4")
    ),
    column(
      width = 4,
      baugeOutput(outputId = "gauge2")
    ),
    column(
      width = 4,
      baugeOutput(outputId = "gauge3")
    )
  ),
  actionButton(inputId = "update", label = "Update values"),
  actionButton(inputId = "update_max", label = "Update max")
)



# server ----

server <- function(input, output, session) {
  
  output$gauge1 <- renderBauge({
    input$update
    bauge(round(sample.int(100, 1)), subtitle = "of total")
  })
  
  output$gauge2 <- renderBauge({
    input$update
    bauge(round(sample.int(100, 1)), full_circle = TRUE)
  })
  
  output$gauge3 <- renderBauge({
    input$update
    bauge(round(sample.int(100, 1)), gauge_width = 20, label_format = suffix(" km/h"))
  })
  
  output$gauge4 <- renderBauge({
    input$update
    input$update_max
    max <- sample(c(100, 150, 200), 1)
    bauge(
      value = round(sample.int(100, 1)),
      max = max,
      steps = c(30, 60, 90, 100),
      colors = c("#FF0000", "#F97600", "#F6C600", "#60B044")
    )
  })
}


# run ----

shinyApp(ui = ui, server = server)
