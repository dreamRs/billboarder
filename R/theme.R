
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


#' @importFrom htmltools htmlDependency
theme_dependency <- function() {
  htmlDependency(
    name = "billboard-theme", 
    version = "1.9.2", 
    src = "htmlwidgets/lib/billboard-1.9",
    package = "billboarder",
    stylesheet = getOption(
      x = "billboard.theme", 
      default = "billboard.min.css"
    )
  )
}