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
#' @importFrom htmlwidgets createWidget sizingPolicy
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
  createWidget(
    name = "billboarder",
    x = x,
    width = width,
    height = height,
    package = "billboarder",
    elementId = elementId, 
    dependencies = billboard_dependencies(),
    sizingPolicy = sizingPolicy(
      defaultWidth = "100%",
      defaultHeight = "100%",
      viewer.defaultHeight = "100%",
      viewer.defaultWidth = "100%",
      browser.defaultHeight = "100%",
      browser.defaultWidth = "100%",
      knitr.defaultHeight = "320px",
      knitr.figure = FALSE,
      browser.fill = TRUE,
      padding = 10
    )
  )
}

#' @importFrom htmltools tags
billboarder_html <- function(id, style, class, ...) {
  tags$div(
    style = style, class = class,
    style = "position: relative;",
    tags$a(
      id = paste0(id, "-export"),
      style = "position:absolute; top:0; right:0; display:none; z-index:50;"
    ),
    tags$div(id = id, class = class, style = style, ...)
  )
}

#' @importFrom htmltools htmlDependency
billboard_dependencies <- function() {
  theme <- getOption(
    x = "billboard.theme", 
    default = "billboard.min.css"
  )
  theme <- paste0("billboard/", theme)
  palette <- getOption(x = "billboard.palette", default = NULL)
  if (!is.null(palette)) {
    palette <- sprintf("<style>.bb-color-pattern {background-image: url('%s') !important;}</style>", palette)
  }
  htmlDependency(
    name = "billboard", 
    version = "3.13.0", 
    src = c(file = "htmlwidgets/lib"),
    package = "billboarder",
    script = "billboard/billboard.pkgd.min.js",
    stylesheet = c(theme, "billboarder.css"), 
    all_files = FALSE,
    head = palette
  )
}


