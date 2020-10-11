#' Shiny bindings for billboarder
#'
#' Output and render functions for using billboarder within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a billboarder
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#' @param shinyId single-element character vector indicating the output ID of the
#'   chart to modify (if invoked from a Shiny module, the namespace will be added
#'   automatically)
#' @param data A \code{data.frame}.
#' @param session the Shiny session object to which the chart belongs; usually the
#'   default value will suffice
#'
#' @name billboarder-shiny
#' 
#' @seealso \code{\link{proxy_example}}
#'
#' @export
#' @importFrom htmlwidgets shinyWidgetOutput shinyRenderWidget
#' 
#' @examples 
#' if (interactive()) {
#'   library(shiny)
#'   
#'   ui <- fluidPage(
#'     tags$h2("Include billboard charts in Shiny"),
#'     fluidRow(
#'       column(
#'         width = 6,
#'         billboarderOutput("mybb1"),
#'         tags$p("Click on a bar to get the value:"),
#'         verbatimTextOutput("res_click")
#'       ),
#'       column(
#'         width = 6,
#'         billboarderOutput("mybb2")
#'       )
#'     )
#'   )
#'   
#'   server <- function(input, output, session) {
#'     
#'     output$mybb1 <- renderBillboarder({
#'       
#'       dat <- data.frame(
#'         label = paste("Label", 1:5),
#'         value = sample.int(100, 5)
#'       )
#'       
#'       billboarder() %>%
#'         bb_barchart(
#'           data = dat,
#'           mapping = bbaes(label, value),
#'           rotated = TRUE
#'         )
#'     })
#'     
#'     output$res_click <- renderPrint({
#'       input$mybb1_click
#'     })
#'     
#'     
#'     output$mybb2 <- renderBillboarder({
#'       
#'       data(AirPassengers)
#'       
#'       air_passengers <- data.frame(
#'         date = as.Date(paste(
#'           rep(1949:1960, each = 12),
#'           rep(1:12, times = 12),
#'           "01", sep = "-"
#'         )), 
#'         passengers = AirPassengers
#'       )
#'       
#'       billboarder() %>% 
#'         bb_linechart(
#'           data = air_passengers, 
#'           mapping = bbaes(date, passengers), type = "spline"
#'         ) %>% 
#'         bb_x_axis(tick = list(format = "%Y", fit = FALSE))
#'     })
#'     
#'   }
#'   
#'   shinyApp(ui, server)
#' }
billboarderOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'billboarder', width, height, package = 'billboarder')
}

#' @rdname billboarder-shiny
#' @export
renderBillboarder <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, billboarderOutput, env, quoted = TRUE)
}


#' @rdname billboarder-shiny
#' @export
billboarderProxy <- function(shinyId, data = NULL, session = shiny::getDefaultReactiveDomain()) {
  
  if (is.null(session)) {
    stop("billboarderProxy must be called from the server function of a Shiny app")
  }
  
  if (!is.null(session$ns) && nzchar(session$ns(NULL)) && substring(shinyId, 1, nchar(session$ns(""))) != session$ns("")) {
    shinyId <- session$ns(shinyId)
  }
  
  structure(
    list(
      session = session,
      id = shinyId,
      x = structure(
        list(data = data)
      )
    ),
    class = "billboarder_Proxy"
  )
}




#' Call a proxy method
#'
#' @param proxy  A \code{billboardProxy} \code{htmlwidget} object.
#' @param name Proxy method.
#' @param ... Arguments passed to method.
#'
#' @return A \code{billboardProxy} \code{htmlwidget} object.
#' @noRd
.bb_proxy <- function(proxy, name, ...) {
  if (!"billboarder_Proxy" %in% class(proxy)) 
    stop("This function must be used with a billboarderProxy object")
  proxy$session$sendCustomMessage(
    type = sprintf("update-billboard-%s", name),
    message = list(id = proxy$id, data = list(...))
  )
  proxy
}
.bb_proxy2 <- function(proxy, name, l) {
  if (!"billboarder_Proxy" %in% class(proxy)) 
    stop("This function must be used with a billboarderProxy object")
  proxy$session$sendCustomMessage(
    type = sprintf("update-billboard-%s", name),
    message = list(id = proxy$id, data = l)
  )
  proxy
}



#' Load data to the chart with proxy
#'
#' @param proxy A \code{billboardProxy} \code{htmlwidget} object.
#' @param data A \code{data.frame} with updated data.
#' @param unload Ids (names) to data to unload.
#' @param ... Arguments passed to method.
#'
#' @return A \code{billboardProxy} \code{htmlwidget} object.
#' @export
bb_load <- function(proxy, data = NULL, unload = NULL, ...) {
  if (!is.null(data)) {
    if (nrow(data) == 1) {
      json <- lapply(X = as.list(data), FUN = list)
    } else {
      json <- as.list(data)
    }
  } else {
    json <- NULL
  }
  
  if (!is.null(unload)) {
    if (length(unload) == 1 && !is.logical(unload)) {
      unload <- list(unload)
    }
  }
  .bb_proxy2(proxy, "load", dropNulls(c(list(json = json, unload = unload), list(...))))
}



#' Unload data to the chart with proxy
#'
#' @param proxy A \code{billboardProxy} \code{htmlwidget} object.
#' @param ids Data ids to unload.
#'
#' @return A \code{billboardProxy} \code{htmlwidget} object.
#' @export
bb_unload <- function(proxy, ids = NULL) {
  if (!is.null(ids)) {
    proxy$unload <- ids
  } else {
    proxy$unload <- TRUE
  }
  proxy
}



#' Highlights specified targets and fade out the others.
#'
#' @param proxy A \code{billboardProxy} \code{htmlwidget} object.
#' @param ids Data ids (names) to be highlighted, if \code{NULL} all datas will be highlighted.
#' 
#' @note \code{bb_defocus} is the opposite of \code{bb_focus}
#'
#' @return A \code{billboardProxy} \code{htmlwidget} object.
#' @export
#' 
#' @name bb_proxy_focus
#' 
#' @examples 
#' if (interactive()) {
#' library("shiny")
#' library("billboarder")
#' 
#' ui <- fluidPage(
#'   tags$h1("Proxy method to highlight data"),
#'   checkboxGroupInput(
#'     inputId = "focus", 
#'     label = "Focus", 
#'     choices = c("setosa", "versicolor", "virginica"), 
#'     inline = TRUE
#'   ),
#'   billboarderOutput(outputId = "bb")
#' )
#' 
#' server <- function(input, output, session) {
#'   
#'   output$bb <- renderBillboarder({
#'     billboarder() %>% 
#'       bb_scatter(
#'         data = iris, 
#'         x = "Sepal.Length", 
#'         y = "Sepal.Width", 
#'         group = "Species"
#'       ) %>% 
#'       bb_axis(x = list(tick = list(fit = FALSE))) %>% 
#'       bb_point(r = 8)
#'   })
#'   
#'   observeEvent(input$focus, {
#'     billboarderProxy("bb") %>% 
#'       bb_proxy_focus(input$focus)
#'   }, ignoreNULL = FALSE)
#' }
#' 
#' shinyApp(ui = ui, server = server)
#' }
bb_proxy_focus <- function(proxy, ids = NULL) {
  if (is.null(ids)) 
    ids <- character(0)
  .bb_proxy2(proxy, "focus", list(ids = ids))
}

#' @rdname bb_proxy_focus
#' @export
bb_proxy_defocus <- function(proxy, ids = NULL) {
  if (is.null(ids)) 
    ids <- character(0)
  .bb_proxy2(proxy, "defocus", list(ids = ids))
}






#' Update axis labels with proxy
#'
#' @param proxy A \code{billboardProxy} \code{htmlwidget} object.
#' @param x X axis label.
#' @param y Y axis label.
#'
#' @return A \code{billboardProxy} \code{htmlwidget} object.
#' @export
bb_proxy_axis_labels <- function(proxy, x = NULL, y = NULL) {
  .bb_proxy2(proxy, "axis_labels", dropNulls(list(x = x, y = y)))
}


#' Update x values with proxy
#'
#' @param proxy A \code{billboardProxy} \code{htmlwidget} object.
#' @param xs Named list of vector(s) used for x values.
#'
#' @return A \code{billboardProxy} \code{htmlwidget} object.
#' @export
bb_proxy_xs <- function(proxy, xs) {
  .bb_proxy(proxy, "xs", dropNulls(xs))
}




#' Update chart type with proxy
#'
#' @param proxy A \code{billboardProxy} \code{htmlwidget} object.
#' @param type Specify the type to be transformed.
#' @param targetIds Specify targets to be transformed. If not given, all targets will be the candidate.
#'
#' @return A \code{billboardProxy} \code{htmlwidget} object.
#' @export
bb_proxy_transform <- function(proxy, type, targetIds = NULL) {
  warning("bb_proxy_transform: this function has been deprecated as of version 0.3.0 and is no longer available.")
}


#' Update chart groups with proxy
#'
#' @param proxy A \code{billboardProxy} \code{htmlwidget} object.
#' @param ... Vector(s) with id of the series, e.g. the name of variables.
#'
#' @return A \code{billboardProxy} \code{htmlwidget} object.
#' @export
bb_proxy_groups <- function(proxy, ...) {
  .bb_proxy2(proxy, "groups", list(...))
}

#' Hide method with proxy
#'
#' @param proxy A \code{billboardProxy} \code{htmlwidget} object.
#' @param targetIdsValue Name of series to hide.
#' @param options Additional options.
#' 
#' @seealso \code{\link{bb_proxy_show}}
#'
#' @return A \code{billboardProxy} \code{htmlwidget} object.
#' @export
bb_proxy_hide <- function(proxy, targetIdsValue, options = NULL) {
  if (is.null(options))
    options <- list()
  .bb_proxy(proxy, "hide", targetIdsValue = targetIdsValue, options = options)
}

#' Show method with proxy
#'
#' @param proxy A \code{billboardProxy} \code{htmlwidget} object.
#' @param targetIdsValue Name of series to show.
#' @param options Additional options.
#' 
#' @seealso \code{\link{bb_proxy_hide}}
#'
#' @return A \code{billboardProxy} \code{htmlwidget} object.
#' @export
bb_proxy_show <- function(proxy, targetIdsValue, options = NULL) {
  if (is.null(options))
    options <- list()
  .bb_proxy(proxy, "show", targetIdsValue = targetIdsValue, options = options)
}



#' Show or hide legend with proxy
#'
#' @param proxy A \code{billboardProxy} \code{htmlwidget} object.
#' @param what \code{show} or \code{hide} the legend.
#' @param targetIds Series ids to show/hide, if \code{NULL} show/hide all legend.
#' 
#' @return A \code{billboardProxy} \code{htmlwidget} object.
#' @export
#' 
#' @examples 
#' if (interactive()) {
#'   library("shiny")
#' 
#'   data("prod_par_filiere")
#' 
#'   ui <- fluidPage(
#'     tags$h2("Show or hide legend with Proxy"),
#'     fluidRow(
#'       column(
#'         width = 3,
#'         checkboxInput(
#'           inputId = "show_legend",
#'           label = "Show legend",
#'           value = TRUE
#'         ),
#'         checkboxGroupInput(
#'           inputId = "item_show",
#'           label = "Item to show in legend",
#'           choices = c("Hydraulic" = "prod_hydraulique",
#'                       "Wind" = "prod_eolien",
#'                       "Solar" = "prod_solaire"),
#'           selected = c("prod_hydraulique", "prod_eolien", "prod_solaire")
#'         )
#'       ),
#'       column(
#'         width = 9,
#'         billboarderOutput(outputId = "mybb")
#'       )
#'     )
#'   )
#' 
#'   server <- function(input, output, session) {
#' 
#'     output$mybb <- renderBillboarder({
#'       billboarder() %>%
#'         bb_barchart(
#'           data = prod_par_filiere[, c(
#'             "annee", "prod_hydraulique", 
#'             "prod_eolien", "prod_solaire"
#'           )],
#'           stacked = TRUE
#'         ) %>%
#'         bb_data(
#'           names = list(prod_hydraulique = "Hydraulic",
#'                        prod_eolien = "Wind",
#'                        prod_solaire = "Solar"),
#'           labels = TRUE
#'         ) %>%
#'         bb_colors_manual(
#'           "prod_eolien" = "#41AB5D",
#'           "prod_hydraulique" = "#4292C6", 
#'           "prod_solaire" = "#FEB24C"
#'         ) %>%
#'         bb_y_grid(show = TRUE) %>%
#'         bb_y_axis(
#'           tick = list(format = suffix("TWh")),
#'           label = list(text = "production (in terawatt-hours)",
#'                        position = "outer-top")
#'         ) %>%
#'         bb_legend(position = "right") %>%
#'         bb_labs(
#'           title = "Renewable energy production",
#'           caption = "Data source: RTE (https://opendata.rte-france.com)"
#'         )
#'     })
#' 
#'     observe({
#'       if (input$show_legend) {
#'         billboarderProxy("mybb") %>% 
#'           bb_proxy_legend(what = "show")
#'       } else {
#'         billboarderProxy("mybb") %>% 
#'           bb_proxy_legend(what = "hide")
#'       }
#'     })
#' 
#'     observe({
#'       lapply(
#'         X = c("prod_hydraulique", "prod_eolien", "prod_solaire"),
#'         FUN = function(x) {
#'           if (x %in% input$item_show) {
#'             billboarderProxy("mybb") %>%
#'               bb_proxy_legend(what = "show", targetIds = x)
#'           } else {
#'             billboarderProxy("mybb") %>%
#'               bb_proxy_legend(what = "hide", targetIds = x)
#'           }
#'         }
#'       )
#'     })
#' 
#'   }
#' 
#'   shinyApp(ui = ui, server = server)
#' }
bb_proxy_legend <- function(proxy, what = c("show", "hide"), targetIds = NULL) {
  what <- match.arg(what)
  if (what == "show") {
    .bb_proxy(proxy, "legend-show", targetIds = targetIds)
  } else {
    .bb_proxy(proxy, "legend-hide", targetIds = targetIds)
  }
}




#' Show or hide tooltip with proxy
#'
#' @param proxy A \code{billboardProxy} \code{htmlwidget} object.
#' @param what \code{show} or \code{hide} the legend.
#' @param x x value on which the tooltip must appear.
#' @param index Index on the x-axis on which the tooltip must appear.
#' @param ... Additional arguments passed to method.
#' 
#' @return A \code{billboardProxy} \code{htmlwidget} object.
#' @export
bb_proxy_tooltip <- function(proxy, what = c("show", "hide"), x = NULL, index = NULL, ...) {
  what <- match.arg(what)
  data <- list(x = x, index = index, ...)
  data <- dropNulls(data)
  if (what == "show") {
    .bb_proxy(proxy, "tooltip-show", data = data)
  } else {
    .bb_proxy(proxy, "tooltip-hide", data = list())
  }
}


#' Change names of the data with proxy
#'
#' @param proxy A \code{billboardProxy} \code{htmlwidget} object.
#' @param old Old names
#' @param new New names
#'
#' @return A \code{billboardProxy} \code{htmlwidget} object.
#' @export
#' 
#' @examples 
#' if (interactive()) {
#' 
#' library(shiny)
#' library(billboarder)
#' 
#' ui <- fluidPage(
#'   tags$h2("Update axis title & data name (tooltip & legend)"),
#'   billboarderOutput(outputId = "my_bb"),
#'   textInput(
#'     inputId = "new_name",
#'     label = "New name :",
#'     value = "this is a new name",
#'     width = "100%"
#'   ),
#'   actionButton(
#'     inputId = "update",
#'     label = "Update chart",
#'     width = "100%"
#'   )
#' )
#' 
#' server <- function(input, output, session) {
#'   
#'   output$my_bb <- renderBillboarder({
#'     dat <- sample(letters[1:5], 100, TRUE)
#'     billboarder() %>%
#'       bb_barchart(data = table(dat)) %>% 
#'       bb_y_axis(label = list(text = "Freq"))
#'   })
#'   
#'   observeEvent(input$update, {
#'     dat <- sample(letters[1:5], 100, TRUE)
#'     billboarderProxy(shinyId = "my_bb") %>% 
#'       bb_proxy_axis_labels(y = input$new_name) %>% 
#'       bb_proxy_data_names(old = "Freq", 
#'                           new = input$new_name) %>% 
#'       bb_barchart(data = table(dat))
#'   }, ignoreInit = TRUE)
#'   
#' }
#' 
#' shinyApp(ui, server)
#' 
#' }
bb_proxy_data_names <- function(proxy, old = NULL, new = NULL) {
  data_names <- as.list(new)
  names(data_names) <- old
  .bb_proxy(proxy, "data-names", names = data_names)
}



#' Change colors with proxy
#'
#' @param proxy A \code{billboardProxy} \code{htmlwidget} object.
#' @param names Names of series
#' @param colors New colors, in same order that \code{names}.
#'
#' @return A \code{billboardProxy} \code{htmlwidget} object.
#' @export
#'
#' @examples
#' if (interactive()) {
#' 
#' library(shiny)
#' library(billboarder)
#' 
#' ui <- fluidPage(
#'   tags$h2("Update colors"),
#'   fluidRow(
#'     column(
#'       width = 3,
#'       selectizeInput(
#'         inputId = "col_eol",
#'         label = "Color for 'prod_eolien':",
#'         choices = c("#66C2A5", "#FC8D62", 
#'                     "#8DA0CB", "#E78AC3",
#'                     "#A6D854", "#FFD92F", 
#'                     "#E5C494", "#B3B3B3")
#'       ),
#'       selectizeInput(
#'         inputId = "col_sol",
#'         label = "Color for 'prod_solaire':",
#'         choices = c("#66C2A5", "#FC8D62", 
#'                     "#8DA0CB", "#E78AC3",
#'                     "#A6D854", "#FFD92F", 
#'                     "#E5C494", "#B3B3B3")
#'       )
#'     ),
#'     column(
#'       width = 9,
#'       billboarderOutput(outputId = "my_bb")
#'     )
#'   )
#' )
#' 
#' server <- function(input, output, session) {
#' 
#'   output$my_bb <- renderBillboarder({
#'     data(prod_par_filiere)
#'     billboarder() %>%
#'       bb_barchart(
#'         data = prod_par_filiere[, c(1, 6, 8)]
#'       )
#'   })
#' 
#'   observe({
#'     billboarderProxy(shinyId = "my_bb") %>%
#'       bb_proxy_data_colors(
#'         names = c("prod_eolien", "prod_solaire"),
#'         colors = c(input$col_eol, input$col_sol)
#'       )
#'   })
#' 
#' }
#' 
#' shinyApp(ui, server)
#' 
#' }
bb_proxy_data_colors <- function(proxy, names = NULL, colors = NULL) {
  data_colors <- as.list(colors)
  names(data_colors) <- names
  .bb_proxy(proxy, "data-colors", colors = data_colors)
}





#' Update chart flow with proxy
#'
#' @param proxy A \code{billboardProxy} \code{htmlwidget} object.
#' @param ... Arguments passed to the flow API, see \url{https://naver.github.io/billboard.js/release/latest/doc/Chart.html#flow}.
#'
#' @return A \code{billboardProxy} \code{htmlwidget} object.
#' @export
#' 
#' @examples 
#' if (interactive()) {
#'   library(shiny)
#'   library(billboarder)
#'   
#'   ui <- fluidPage(
#'     tags$h3("Proxy flow"),
#'     actionButton(
#'       inputId = "next_data",
#'       label = "Add data",
#'       icon = icon("arrow-right")
#'     ),
#'     billboarderOutput(outputId = "chart1"),
#'     
#'     tags$h4("Real time chart"),
#'     billboarderOutput(outputId = "chart2")
#'   )
#'   
#'   server <- function(input, output, session) {
#'     
#'     
#'     time_data <- reactiveValues(df = data.frame(
#'       x = Sys.Date() + 1:20,
#'       y = round(rnorm(20) * 10)
#'     ))
#'     
#'     output$chart1 <- renderBillboarder({
#'       billboarder() %>% 
#'         bb_linechart(data = isolate(time_data$df))
#'     })
#'     
#'     observeEvent(input$next_data, {
#'       time_data$df$x <- time_data$df$x + 21
#'       time_data$df$y <- round(rnorm(20) * 10)
#'     })
#'     
#'     observe({
#'       billboarderProxy("chart1") %>% 
#'         bb_proxy_flow(json = as.list(time_data$df), duration = 1500)
#'     })
#'     
#'     
#'     
#'     output$chart2 <- renderBillboarder({
#'       df <- data.frame(
#'         x = Sys.time() - 1:20 * 2,
#'         y = round(rnorm(20) * 10)
#'       )
#'       billboarder() %>% 
#'         bb_linechart(data = df) %>% 
#'         bb_x_axis(tick = list(format = "%H:%M", fit = FALSE))
#'     })
#'     
#'     observe({
#'       invalidateLater(2000)
#'       billboarderProxy("chart2") %>% 
#'         bb_proxy_flow(json = list(
#'           x = list(format(Sys.time())),
#'           y = list(round(rnorm(1) * 10))
#'         ), data = list(x = "x"))
#'     })
#'     
#'   }
#'   
#'   shinyApp(ui, server)
#' }
bb_proxy_flow <- function(proxy, ...) {
  .bb_proxy2(proxy, "flow", list(...))
}


