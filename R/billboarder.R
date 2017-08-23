#' Create a Billboard.js widget
#'
#' Create an interactive visualization with Javascript library Billboard.js
#'
#' @param bb_opts A \code{list} in JSON format with chart parameters,
#'  see \url{https://naver.github.io/billboard.js/demo/}.
#' @param data A \code{data.frame}.
#' @param width A numeric input in pixels.
#' @param height A numeric input in pixels.
#' @param elementId Use an explicit element ID for the widget.
#'
#' @import htmlwidgets
#' @importFrom magrittr %>%
#'
#' @export
billboarder <- function(bb_opts = list(), data = NULL, width = NULL, height = NULL, elementId = NULL) {

  # disabling touch events for Rstudio
  # https://github.com/naver/billboard.js/issues/92
  if (is.null(bb_opts$interaction$inputType$touch))
    bb_opts$interaction$inputType$touch <- FALSE

  bb_empty <- getOption(x = "bb.empty")
  if (is.function(bb_empty))
    bb_empty <- bb_empty()
  
  x <- list(
    bb_opts = bb_opts,
    bb_empty = bb_empty,
    data = data
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
      defaultWidth = "95%",
      viewer.defaultHeight = "100%",
      viewer.defaultWidth = "100%",
      knitr.figure = FALSE,
      browser.fill = TRUE,
      padding = 10
    )
  )
}

