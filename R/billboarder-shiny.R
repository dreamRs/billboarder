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
#' @param session the Shiny session object to which the chart belongs; usually the
#'   default value will suffice
#'
#' @name billboarder-shiny
#'
#' @export
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
billboarderProxy <- function(shinyId, session = shiny::getDefaultReactiveDomain()) {
  
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
        list()
      )
    ),
    class = "billboarder_Proxy"
  )
}



#' Retrieve click value in Shiny
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param inputId The \code{input} slot that will be used to access the value.
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @noRd
#' 
#' @importFrom htmlwidgets JS
#'
#' @examples
#' \dontrun{
#' 
#'  if (interactive()) {
#'  
#'  
#'  }
#' 
#' }
bb_click <- function(bb, inputId) {
  
  click <- sprintf("Shiny.onInputChange('%s', d);", inputId)
  click <- paste0("function(d, element) {", click, "}")
  click <- htmlwidgets::JS(click)
  
  bb <- .bb_opt(bb, "data", onclick = click)
  return(bb)
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
  
  proxy$session$sendCustomMessage(
    type = sprintf("update-billboard-%s", name),
    message = list(id = proxy$id, data = list(...))
  )
  
  proxy
}
.bb_proxy2 <- function(proxy, name, l) {
  
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
  
  if (!"billboarder_Proxy" %in% class(proxy)) 
    stop("This function must be used with a billboarderProxy object")
  
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
    if (length(unload) == 1) {
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
  
  if (!"billboarder_Proxy" %in% class(proxy)) 
    stop("This function must be used with a billboarderProxy object")
  
  .bb_proxy2(proxy, "unload", dropNulls(list(ids = ids)))
  
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
#' @name bb_focus
#' 
#' @examples 
#' \dontrun{
#' if (interactive()) {
#' library("shiny")
#' library("billboarder")
#' library("magrittr")
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
#'       bb_focus(input$focus)
#'   }, ignoreNULL = FALSE)
#' }
#' 
#' shinyApp(ui = ui, server = server)
#' }
#' }
bb_focus <- function(proxy, ids = NULL) {
  
  if (!"billboarder_Proxy" %in% class(proxy)) 
    stop("This function must be used with a billboarderProxy object")
  
  if (is.null(ids)) 
    ids <- character(0)
  
  .bb_proxy2(proxy, "focus", list(ids = ids))
  
}

#' @rdname bb_focus
#' @export
bb_defocus <- function(proxy, ids = NULL) {
  
  if (!"billboarder_Proxy" %in% class(proxy)) 
    stop("This function must be used with a billboarderProxy object")
  
  if (is.null(ids)) 
    ids <- character(0)
  
  .bb_proxy2(proxy, "defocus", list(ids = ids))
  
}