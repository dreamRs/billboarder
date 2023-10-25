
#' Set theme and default colors for Billboard charts
#'
#' @param name Name of the theme, possible values are : `"billboard"`,
#'  `"insight"`, `"graph"`, `"datalab"`, `"modern"`.
#' 
#' @note You can only use one theme and palette at a time (in Shiny applications or Markdown documents).
#'
#' @export
#' 
#' @name billboard-theme
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
set_theme <- function(name = c("billboard", "insight", "graph", "datalab", "modern")) {
  name <- match.arg(arg = name)
  options("billboard.theme" = paste0(name, ".min.css"))
}

#' @param colors Vector of colors to use as default.
#' @export
#' @rdname billboard-theme
set_color_palette <- function(colors) {
  colors <- paste(colors, collapse = ";")
  options("billboard.palette" = colors)
}



