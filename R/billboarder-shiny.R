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
    stop("leafletProxy must be called from the server function of a Shiny app")
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
#' @export
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



.bb_proxy <- function(proxy, name, ...) {
  
  proxy$session$sendCustomMessage(
    type = sprintf("update-billboard-%s", name),
    message = list(id = proxy$id, data = list(...))
  )
  
  proxy
}

