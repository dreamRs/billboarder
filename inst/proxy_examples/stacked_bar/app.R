
library("shiny")
library("billboarder")

# data ----
Titanic <- as.data.frame(Titanic)

# ui ----

ui <- fluidPage(
  
  tags$h1("Update stacked bar chart with proxy"),
  br(),
  
  tags$h3("Update variable on x-axis"),
  fluidRow(
    column(
      width = 3,
      wellPanel(
        radioButtons(
          inputId = "update_x", 
          label = "Choose a variable for x-axis:", 
          choices = colnames(Titanic)[-c(4, 5)],
          selected = "Sex"
        )
      )
    ),
    column(
      width = 9,
      billboarderOutput(outputId = "bb_xaxis")
    )
  ),
  
  tags$h3("Update grouping variable"),
  fluidRow(
    column(
      width = 3,
      wellPanel(
        radioButtons(
          inputId = "update_group", 
          label = "Choose a variable for x-axis:", 
          choices = colnames(Titanic)[-c(4, 5)],
          selected = "Sex"
        )
      )
    ),
    column(
      width = 9,
      billboarderOutput(outputId = "bb_group")
    )
  )
)


# server ----

server <- function(input, output, session) {
  
  output$bb_xaxis <- renderBillboarder({
    formul_ <- paste0("Freq ~ ", isolate(input$update_x), " + Survived")
    dat <- aggregate(formula = as.formula(formul_), data = Titanic, FUN = sum)
    billboarder(data = dat) %>% 
      bb_barchart(mapping = bbaes_string(x = isolate(input$update_x), y = "Freq", group = "Survived"))
  })
  
  observeEvent(input$update_x, {
    formul_ <- paste0("Freq ~ ", input$update_x, " + Survived")
    dat <- aggregate(formula = as.formula(formul_), data = Titanic, FUN = sum)
    billboarderProxy(shinyId = "bb_xaxis", data = dat) %>% 
      bb_unload() %>% 
      bb_aes_string(x = input$update_x, y = "Freq", group = "Survived") %>% 
      bb_barchart()
  }, ignoreInit = TRUE)
  
  
  
  output$bb_group <- renderBillboarder({
    formul_ <- paste0("Freq ~ ", isolate(input$update_group), " + Survived")
    dat <- aggregate(formula = as.formula(formul_), data = Titanic, FUN = sum)
    billboarder(data = dat) %>% 
      bb_barchart(mapping = bbaes_string(x = "Survived", y = "Freq", group = isolate(input$update_group)))
  })
  
  observeEvent(input$update_group, {
    formul_ <- paste0("Freq ~ ", input$update_group, " + Survived")
    dat <- aggregate(formula = as.formula(formul_), data = Titanic, FUN = sum)
    billboarderProxy(shinyId = "bb_group", data = dat) %>% 
      bb_unload() %>%
      bb_aes_string(x = "Survived", y = "Freq", group = input$update_group) %>% 
      bb_barchart()
  }, ignoreInit = TRUE)
  
  
  shiny::onStop(shiny::stopApp)
}


# run app ----

shinyApp(ui = ui, server = server)



