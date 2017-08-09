

# Packages ----
library("shiny")
library("eurostat")
library("dplyr")
library("tidyr")
library("billboarder")


# Data ----
dat <- get_eurostat(id = "tsdtr210", time_format = "num", filters = list(geo = eu_countries$code))
# datl <- label_eurostat(dat)
trains <- dat %>% filter(vehicle == "TRN")


# App ----
ui <- fluidPage(
  fluidRow(
    column(
      width = 8, offset = 2,
      tags$h1("Train passengers for selected EU countries in 1990 - 2012"),
      billboarderOutput(outputId = "eu_train_passengers"),
      sliderInput(inputId = "time", label = "Year", min = 1990, max = 2012, 
                  value = 1990, animate = TRUE, width = "100%", sep = "")
    )
  )
)

server <- function(input, output, session) {
  
  output$eu_train_passengers <- renderBillboarder({
    
    billboarder() %>% 
      bb_barchart(data = trains %>% filter(time == 1990) %>% select(geo, values)) %>% 
      bb_legend(show = FALSE) %>% 
      bb_y_grid(show = TRUE) %>% 
      bb_y_axis(tick = list(format = suffix("%")), max = max(trains$values, na.rm = TRUE)) %>% 
      bb_labs(caption = "Data source: Eurostat (via the `eurostat` package)", y = "% in total inland passenger-km")
    
  })
  
  
  observeEvent(input$time, {
    
    billboarderProxy(shinyId = "eu_train_passengers") %>% 
      bb_barchart(data = trains %>% filter(time == input$time) %>% select(geo, values))
    
  }, ignoreInit = TRUE)
  
}

shinyApp(ui = ui, server = server)
