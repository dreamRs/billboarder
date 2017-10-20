
# Packages ----
library("shiny")
library("billboarder")


# Data ----
data("mpg", package = "ggplot2")
mpg$weight <- 1


# ui ----
ui <- fluidPage(
  tags$h1("Proxy for lollipop"),
  fluidRow(
    column(
      width = 3,
      wellPanel(
        radioButtons(
          inputId = "year", 
          label = "Year:", 
          choices = c("1999", "2008", "Both"),
          selected = "Both"
        ),
        sliderInput(
          inputId = "cty",
          label = "Cty:", 
          min = min(mpg$cty), 
          max = max(mpg$cty), 
          value = range(mpg$cty),
          step = 1
        )
      )
    ),
    column(
      width = 9,
      billboarderOutput(outputId = "lollipop")
    )
  )
)


# server ----
server <- function(input, output, session) {
  
  output$lollipop <- renderBillboarder({
    billboarder(data = mpg) %>% 
        bb_lollipop(mapping = bbaes(x = class, y = weight), rotated = TRUE) %>% 
      bb_y_grid(show = TRUE) %>% 
      bb_y_axis(label = "Number of models", max = 65) %>% 
      bb_x_axis(tick = list(centered = TRUE))
  })
  
  observeEvent(list(input$year, input$cty), {
    
    if (input$year == "Both") {
      year <- c("1999", "2008")
    } else {
      year <- input$year
    }
    
    mpg <- mpg[
      mpg$year %in% year & mpg$cty >= input$cty[1] & mpg$cty <= input$cty[2], 
    ]
    
    billboarderProxy(shinyId = "lollipop") %>% 
      bb_lollipop(data = mpg, mapping = bbaes(x = class, y = weight))
    
  }, ignoreInit = TRUE)
  
  shiny::onStop(shiny::stopApp)
}

# app ----
shinyApp(ui = ui, server = server)
