#' Create a Billboard.js widget
#'
#' Create an interactive visualization with Javascript library Billboard.js.
#'
#' @param bb_opts A `list` in JSON format with chart parameters, see
#'   <https://naver.github.io/billboard.js/demo/>.
#' @param data A `data.frame`.
#' @param width A numeric input in pixels.
#' @param height A numeric input in pixels.
#' @param elementId Use an explicit element ID for the widget.
#'
#' @importFrom htmlwidgets createWidget sizingPolicy
#'
#' @export
#' 
#' @examples
#' # Bar chart
#' stars <- data.frame(
#'   package = c("billboarder", "ggiraph", "officer", "shinyWidgets", "visNetwork"),
#'   stars = c(67, 252, 160, 144, 224)
#' )
#'
#' billboarder() %>%
#'   bb_barchart(data = stars) %>%
#'   bb_y_grid(show = TRUE) %>%
#'   bb_labs(
#'     title = "GitHub stars",
#'     caption = "Example with billboarder"
#'   )
#'
#' # Scatter plot with grouping
#' billboarder(data = iris) %>%
#'   bb_scatterplot(
#'     mapping = bbaes(Sepal.Length, Sepal.Width, group = Species)
#'   ) %>%
#'   bb_point(r = 6) %>%
#'   bb_labs(
#'     title = "Iris dataset",
#'     subtitle = "Sepal length vs sepal width"
#'   ) %>%
#'   bb_x_axis(
#'     label = list(text = "Sepal length", position = "outer-center")
#'   ) %>%
#'   bb_y_axis(
#'     label = list(text = "Sepal width", position = "outer-middle")
#'   )
#'   
#'   
#' # Using raw Billboard.js options
#' billboarder(
#'   bb_opts = list(
#'     data = list(
#'       columns = list(
#'         c("data1", 30, 200, 100, 400, 150, 250),
#'         c("data2", 50, 20, 10, 40, 15, 25)
#'       ),
#'       type = "bar",
#'       colors = list(
#'         data1 = "#1f77b4",
#'         data2 = "#ff7f0e"
#'       )
#'     ),
#'     axis = list(
#'       x = list(
#'         type = "category",
#'         categories = c("Jan", "Feb", "Mar", "Apr", "May", "Jun")
#'       )
#'     ),
#'     grid = list(
#'       y = list(show = TRUE)
#'     )
#'   )
#' )
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
    version = "4.0.3",
    src = c(file = "htmlwidgets/lib"),
    package = "billboarder",
    script = "billboard/billboard.pkgd.min.js",
    stylesheet = c(theme, "billboarder.css"),
    all_files = FALSE,
    head = palette
  )
}
