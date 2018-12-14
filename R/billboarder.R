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
#' @importFrom htmltools htmlDependency
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
    name = 'billboarder',
    x = x,
    width = width,
    height = height,
    package = 'billboarder',
    elementId = elementId, 
    dependencies = htmlDependency(
      name = "billboard-theme", 
      version = "1.7.0", 
      src = "htmlwidgets/lib/billboard-1.7",
      package = "billboarder",
      stylesheet = getOption(
        x = "billboard.theme", 
        default = "billboard.min.css"
      )
    ),
    sizingPolicy = sizingPolicy(
      defaultWidth = "95%",
      viewer.defaultHeight = "100%",
      viewer.defaultWidth = "100%",
      knitr.figure = FALSE,
      browser.fill = TRUE,
      padding = 10
    )
  )
}


#' Set theme for Billboard charts
#'
#' @param name Name of the theme, possible values are : \code{"billboard"}, \code{"insight"}, \code{"graph"}.
#' 
#' @note You can only use one theme at a time (in Shiny applications or Markdown documents).
#'
#' @export
#'
#' @examples
#' library("billboarder")
#' set_theme("insight")
#' 
#' data("prod_par_filiere")
#' billboarder() %>%
#'   bb_barchart(
#'     data = prod_par_filiere[, c("annee", "prod_hydraulique", "prod_eolien", "prod_solaire")]
#'   ) %>%
#'   bb_data(
#'     names = list(prod_hydraulique = "Hydraulic", prod_eolien = "Wind", prod_solaire = "Solar")
#'   ) %>% 
#'   bb_y_grid(show = TRUE) %>%
#'   bb_y_axis(tick = list(format = suffix("TWh")),
#'             label = list(text = "production (in terawatt-hours)", position = "outer-top")) %>% 
#'   bb_legend(position = "inset", inset = list(anchor = "top-right")) %>% 
#'   bb_labs(title = "Renewable energy production",
#'           caption = "Data source: RTE (https://opendata.rte-france.com)")
set_theme <- function(name = c("billboard", "insight", "graph")) {
  name <- match.arg(arg = name)
  options("billboard.theme" = paste0(name, ".min.css"))
}



