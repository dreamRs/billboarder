

# Example proxy bar -------------------------------------------------------

library("shiny")
library("billboarder")
library("data.table")

# data ----
data("mpg", package = "ggplot2")
setDT(mpg)




# ui ----

ui <- fluidPage(
  tags$h1("Proxy method with billboarder"),
  br(),
  fluidRow(
    column(
      width = 3,
      wellPanel(
        selectizeInput(
          inputId = "year",
          label = "year :",
          choices = c("1999", "2008"),
          selected = "1999",
          multiple = TRUE, 
          options = list('plugins' = list('remove_button'))
        ),
        sliderInput(
          inputId = "cty", 
          label = "cty >= x :",
          min = 9, 
          max = 35, 
          value = 9
        ),
        checkboxInput(
          inputId = "keepallx", 
          label = "Keep all x", 
          value = FALSE
        )
      )
    ),
    column(
      width = 9,
      billboarderOutput(outputId = "bb")
    )
  )
)


# server ----

server <- function(input, output, session) {
  
  output$bb <- renderBillboarder({
    billboarder() %>%
      bb_barchart(
        data = dcast(
          data = mpg[, list(count = .N), by = list(manufacturer, year)],
          formula = manufacturer~year,
          value.var = "count"
        )[, list(manufacturer, `1999`)]
      ) %>%
      bb_axis(rotated = TRUE) %>%
      bb_y_grid(show = TRUE) %>% 
      bb_title(text = "Number of models by manufacturer", position = "left-top")
  })
  
  
  observe({
    
    dat <- copy(mpg)
    dat <- dat[cty >= input$cty, list(count = .N), by = list(manufacturer, year)]
    
    if (input$keepallx) {
      dat <- merge(x = unique(mpg[, list(manufacturer)]), y = dat, by = "manufacturer", all.x = TRUE)
    }
    
    dat <- dcast(data = dat, formula = manufacturer~year, value.var = "count")
    
    billboarderProxy("bb") %>% 
      bb_unload(setdiff(c("1999", "2008"), input$year)) %>% 
      bb_barchart(data = dat[, .SD, .SDcols = c("manufacturer", input$year)])
  })
  
  
}


# run app ----

shinyApp(ui = ui, server = server)
