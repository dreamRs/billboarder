
library(shiny)
library(billboarder)
data("economics_long", package = "ggplot2")



ui <- fluidPage(
  tags$h2("Fixed tooltip"),
  fluidRow(
    column(
      width = 8,
      billboarderOutput("mychart")
    ),
    column(
      width = 4,
      tags$div(id = "mytooltip")
    )
  )
)

server <- function(input, output, session) {
  
  output$mychart <- renderBillboarder({
    billboarder(data = economics_long) %>% 
      bb_linechart(
        mapping = bbaes(x = date, y = value01, group = variable)
      ) %>% 
      bb_x_axis(tick = list(fit = FALSE)) %>% 
      bb_tooltip(
        init = list(
          show = TRUE
        ),
        doNotHide = TRUE,
        contents = list(
          bindto = "#mytooltip",
          template = "<ul><li>Index<br>{=TITLE}</li>{{<li class={=CLASS_TOOLTIP_NAME}><span>{=VALUE}</span><br><span style=color:{=COLOR}>{=NAME}</span></li>}}</ul>"
        )
      )
  })
  
}

shinyApp(ui, server)
