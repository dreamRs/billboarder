


library(shiny)
library(billboarder)

ui <- fluidPage(
  fluidRow(
    column(
      width = 8, offset = 2,
      tags$h1("Export billboard"),
      billboarderOutput(outputId = "mybb"),
      actionButton(inputId = "export", label = "Export", icon = icon("download")),
      tags$div(id = "Export2")
    )
  )
)

server <- function(input, output, session) {
  
  output$mybb <- renderBillboarder({
    data("prod_par_filiere")
    billboarder() %>%
      bb_barchart(data = prod_par_filiere[, c("annee", "prod_hydraulique")], color = "#102246") %>%
      bb_y_grid(show = TRUE) %>%
      bb_y_axis(tick = list(format = suffix("TWh")),
                label = list(text = "production (in terawatt-hours)", position = "outer-top")) %>% 
      bb_legend(show = FALSE) %>% 
      bb_labs(title = "French hydraulic production",
              caption = "Data source: RTE (https://opendata.rte-france.com)")
  })
  
  observeEvent(input$export, {
    billboarderProxy(shinyId = "mybb") %>% billboarder:::bb_export()
  })
  
}

shinyApp(ui, server)



