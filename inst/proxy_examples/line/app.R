
library("shiny")
library("billboarder")


# ui ----

ui <- fluidPage(
  
  tags$h1("Billboarder line chart with proxy"),
  br(),
  tags$p(
    "This is the billboarder adaption of example",
    tags$a(
      href = "https://github.com/rstudio/shiny-examples/tree/master/054-nvd3-line-chart-output",
      "054-nvd3-line-chart-output"
    ),
    "by Joe Cheng."
  ),
  
  fluidRow(
    column(width=9,
           billboarderOutput(outputId = "mychart")
    ),
    column(width=3,
           sliderInput(inputId = "sinePhase", label = "Sine phase", 
                       min = -180, max = 180, value = 0, step = 10,
                       animate = animationOptions(interval = 100, loop = TRUE)),
           sliderInput(inputId = "sineAmplitude", label = "Sine amplitude",
                       min = -2, max = 2, value = 1, step = 0.1,
                       animate=animationOptions(interval = 100, loop = TRUE))
    )
  )
)


# server ----

server <- function(input, output, session) {
  
  dat <- reactive({
    data.frame(
      index = seq_len(100),
      Sine = sin(1:100/10 + input$sinePhase * pi/180) * input$sineAmplitude,
      Cosine = 0.5 * cos(1:100/10),
      "Sine 2" = sin(1:100/10) * 0.25 + 0.5
    )
  })
  
  output$mychart <- renderBillboarder({
    billboarder() %>% 
      bb_linechart(data = isolate(dat()), type = "spline")
  })
  
  observe({
    billboarderProxy(shinyId = "mychart") %>% 
      bb_linechart(data = dat())
  })
  
  shiny::onStop(shiny::stopApp)
}


# app ----

shinyApp(ui = ui, server = server)
