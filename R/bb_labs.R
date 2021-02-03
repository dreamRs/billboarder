#' Quickly set title, axis labels and caption
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param title Text for the chart title, use \code{\\n} to make a new line.
#' @param x Text for x axis title.
#' @param y Text for y axis title.
#' @param caption Text for the caption displayed in the bottom-right of the chart.
#' @param caption_href Associate the caption with a link to an URL.
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
#'   bb_barchart(
#'     data = prod_par_filiere[, c("annee", "prod_hydraulique")],
#'     color = "#102246"
#'   ) %>%
#'   bb_legend(show = FALSE) %>%
#'   bb_labs(
#'     title = "French hydraulic production",
#'     y = "production (in terawatt-hours)",
#'     caption = "Data source: RTE (https://opendata.reseaux-energies.fr/)",
#'     caption_href = "https://opendata.reseaux-energies.fr/"
#'   )
bb_labs <- function(bb, title = NULL, x = NULL, y = NULL, caption = NULL, caption_href = NULL) {
  
  if (!is.null(title)) {
    bb <- bb_title(
      bb = bb,
      text = title, 
      position = "left-top"
    )
  }
  
  if (!is.null(x)) {
    bb <- bb_x_axis(
      bb = bb, 
      label = list(text = x, position = "outer-right")
    )
  }
  
  if (!is.null(y)) {
    bb <- bb_y_axis(
      bb = bb, 
      label = list(text = y, position = "outer-top")
    )
  }
  
  if (!is.null(caption)) {
    bb <- .bb_opt2(bb, "caption", l = dropNulls(list(
      text = caption,
      href = caption_href
    )))
    bb <- .bb_opt(bb, "padding", bottom = 10)
  }
  
  return(bb)
}
