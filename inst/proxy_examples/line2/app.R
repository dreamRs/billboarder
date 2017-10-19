
library("shiny")
library("billboarder")
library("RColorBrewer")

# data ----
data("cdc_prod_filiere")

cols <- brewer.pal(n = 9, name = "Set1")
vars <- colnames(cdc_prod_filiere)[3:11]
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
  
  tags$h1("Billboarder line chart with proxy"),
  br(),
  
  fluidRow(
    
    column(
      width = 4,
      
      wellPanel(
        
        radioButtons(
          inputId = "line", label = "Number of lines",
          choices = c("One", "Several"), inline = TRUE
        ),
        
        conditionalPanel(
          condition = "input.line == 'One'",
          radioButtons(
            inputId = "onebranch",
            label = "Branch :",
            choiceNames = choices, 
            choiceValues = vars, 
            selected = colnames(cdc_prod_filiere)[3]
          )
        ),
        
        conditionalPanel(
          condition = "input.line == 'Several'",
          checkboxGroupInput(
            inputId = "severalbranch",
            label = "Branch :",
            choiceNames = choices, 
            choiceValues = vars, 
            selected = colnames(cdc_prod_filiere)[3]
          )
        )

        
      )
      
    ),
    
    column(
      width = 8,
      billboarderOutput(outputId = "courbe")
    )
    
  )
)


# server ----

server <- function(input, output, session) {
  
  # initialize chart
  
  output$courbe <- renderBillboarder({
    billboarder() %>% 
      bb_linechart(data = cdc_prod_filiere[, c(1, 3)]) %>% 
      bb_colors_manual(setNames(as.list(cols), vars)) %>% 
      bb_labs(title = "Electricity production (2017-06-12)",
              y = "In megawatt (MW)",
              caption = "Data source: RTE (https://opendata.rte-france.com)")
  })
  

  observeEvent(input$onebranch, {

    billboarderProxy(shinyId = "courbe") %>%
      bb_unload() %>%
      bb_linechart(data = cdc_prod_filiere[, c("date_heure", input$onebranch)])
    
  }, ignoreInit = TRUE)
  
  
  observeEvent(input$severalbranch, {
    
    billboarderProxy(shinyId = "courbe") %>%
      bb_unload(setdiff(vars, input$severalbranch)) %>%
      bb_linechart(data = cdc_prod_filiere[, c("date_heure", input$severalbranch)])
    
  }, ignoreInit = TRUE)
  
  shiny::onStop(shiny::stopApp)
}


# run app ----

shinyApp(ui = ui, server = server)
