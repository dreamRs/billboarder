
library("shiny")
library("billboarder")



# data ----
data("prod_par_filiere")

cols <- RColorBrewer::brewer.pal(n = 9, name = "Set1")
vars <- colnames(prod_par_filiere)[3:11]
choices <- mapply(
  FUN = function(x, y) {
    tags$span(icon("circle"), style = paste("color:", x), 
              gsub("prod_", "", y))
  },
  x = cols,
  y = vars, 
  SIMPLIFY = FALSE, USE.NAMES = FALSE
)



# ui ----

ui <- fluidPage(
  
  tags$h1("Billboarder bar chart with proxy"),
  br(),

  fluidRow(
    
    column(
      width = 3,
      radioButtons(
        inputId = "branch",
        label = "Branch :",
        choiceNames = choices, 
        choiceValues = vars, 
        selected = vars[1]
      )
    ),
    
    column(
      width=9,
      billboarderOutput(outputId = "mychart")
    )
  )
)


# server ----

server <- function(input, output, session) {
  
  r_cols <- reactiveValues(x = NULL)
  
  dat <- reactive({
    req(input$branch)
    r_cols$x <- cols[which(vars == input$branch)]
    dat <- prod_par_filiere[, c("annee", input$branch)]
    stats::setNames(dat, c("year", "branch"))
  })
  
  output$mychart <- renderBillboarder({
    billboarder() %>%
      bb_barchart(data = isolate(dat()), color = isolate(r_cols$x)) %>% # note the isolate
      bb_y_grid(show = TRUE) %>%
      bb_y_axis(tick = list(format = suffix("TWh")),
                label = list(text = "production (in terawatt-hours)", position = "outer-top")) %>% 
      bb_legend(show = FALSE) %>% 
      bb_labs(title = "French electricity production",
              caption = "Data source: RTE (https://opendata.rte-france.com)")
  })
  
  # update is done here
  observe({
    billboarderProxy(shinyId = "mychart") %>% 
      bb_barchart(data = dat(), color = r_cols$x)
  })
  
  shiny::onStop(shiny::stopApp)
}


# app ----

shinyApp(ui = ui, server = server)
