#' Create a Billboard.js widget
#'
#' Create an interactive visualization with Javascript library Billboard.js
#'
#' @param bb_opts A \code{list} in JSON format with chart parameters, see \url{https://naver.github.io/billboard.js/demo/}.
#' @param width A numeric input in pixels.
#' @param height A numeric input in pixels.
#' @param elementId Use an explicit element ID for the widget.
#'
#' @import htmlwidgets
#'
#' @export
billboarder <- function(bb_opts = list(), width = NULL, height = NULL, elementId = NULL) {


  x <- list(
    bb_opts = bb_opts
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'billboarder',
    x = x,
    width = width,
    height = height,
    package = 'billboarder',
    elementId = elementId,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = "100%",
      viewer.defaultHeight = "100%",
      viewer.defaultWidth = "100%",
      knitr.figure = FALSE,
      browser.fill = TRUE,
      padding = 10
    )
  )
}

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
