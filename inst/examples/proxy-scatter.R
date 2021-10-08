

library("shiny")
library("billboarder")

ui <- fluidPage(
  tags$h1('Iris k-means clustering'),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = 'xcol', label = 'X Variable', choices = names(iris)),
      selectInput(inputId = 'ycol', label = 'Y Variable', choices = names(iris),
                  selected = names(iris)[[2]]),
      numericInput(inputId = 'clusters', label = 'Cluster count', value = 3,
                   min = 1, max = 9)
    ),
    mainPanel(
      billboarderOutput(outputId = 'plot1')
    )
  )
)

server <- function(input, output, session) {
  
  output$plot1 <- renderBillboarder({
    iris$cluster <- paste0("clus", kmeans(iris[, c("Sepal.Length", "Sepal.Width")], 3)$cluster)
    billboarder() %>% 
      bb_scatterplot(data = iris, x = "Sepal.Length", y = "Sepal.Width", group = "cluster") %>% 
      bb_axis(x = list(tick = list(fit = FALSE))) %>%
      bb_legend(show = FALSE) %>% 
      bb_color(palette = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                           "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")) %>% 
      bb_point(r = 6)
  })
  
  
  observeEvent(list(input$xcol, input$ycol, input$clusters), {
    iris$cluster <- paste0("clus", kmeans(iris[, c(input$xcol, input$ycol)], input$clusters)$cluster)
    billboarderProxy(shinyId = "plot1") %>% 
      bb_unload() %>%
      bb_scatterplot(data = iris, x = input$xcol, y = input$ycol, group = "cluster")
  }, ignoreInit = TRUE)
  
}

shinyApp(ui = ui, server = server)