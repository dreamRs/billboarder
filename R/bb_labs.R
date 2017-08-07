#' Quickly set title, axis labels and caption
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param title Plot title.
#' @param x Label for x axis.
#' @param y Label for y axis.
#' @param caption Caption for the chart.
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#' 
#' @note \code{caption} is not part of the billboard.js library, it is added by the \code{billboarder} package.
#'
#' @examples
#' data("prod_par_filiere")
#' 
#' billboarder() %>%
#'   bb_barchart(data = prod_par_filiere[, c("annee", "prod_hydraulique")], color = "#102246") %>%
#'   bb_legend(show = FALSE) %>% 
#'   bb_labs(title = "French hydraulic production", 
#'           y = "production (in terawatt-hours)",
#'           caption = "Data source: RTE (https://opendata.rte-france.com)")
#' 
bb_labs <- function(bb, title = NULL, x = NULL, y = NULL, caption = NULL) {
  
  if (!is.null(title)) {
    bb <- bb_title(bb, text = title, position = "left-top", 
                   padding = list(top = 0, right = 0, left = 0, bottom = 20))
  }
  
  if (!is.null(x)) {
    bb <- bb_x_axis(bb, label = list(text = x, position = "outer-right"))
  }
  
  if (!is.null(y)) {
    bb <- bb_y_axis(bb, label = list(text = y, position = "outer-top"))
  }
  
  if (!is.null(caption)) {
    bb <- .bb_opt(bb, "caption", text = caption)
    bb <- .bb_opt(bb, "padding", bottom = 20)
  }
  
  return(bb)
}