#' Add custom style for regions and grid lines
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param region A named list with style associated with region.
#' @param x_grid_line A named list with style associated with grid line on the X-axis.
#' @param y_grid_line A named list with style associated with grid line on the Y-axis.
#' @param ... Not used
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
#' @examples
#' # Change default color for regions
#' billboarder() %>% 
#'   bb_linechart(data = sin(seq(-pi, pi, length.out = 30))) %>% 
#'   bb_regions(
#'     list(start = 0, end = 10, class = "custom"), # add custom class
#'     list(start = 19, end = 29, class = "foo")
#'   ) %>% 
#'   bb_add_style(region = list(custom = "fill: red;", foo = "fill: #009246;"))
#'
bb_add_style <- function(bb, region = NULL, x_grid_line = NULL, y_grid_line = NULL, ...) {
  
  if (!is.null(region)) {
    region <- paste0(".bb-region.", names(region), "{", unlist(region, use.names = FALSE), "}")
    region <- paste(region, collapse = "")
  }
  if (!is.null(x_grid_line)) {
    x_grid_line <- paste0(".bb-xgrid-line.", names(x_grid_line), " line{", unlist(x_grid_line, use.names = FALSE), "}")
    x_grid_line <- paste(x_grid_line, collapse = "")
  }
  if (!is.null(y_grid_line)) {
    y_grid_line <- paste0(".bb-ygrid-line.", names(y_grid_line), " line{", unlist(y_grid_line, use.names = FALSE), "}")
    y_grid_line <- paste(y_grid_line, collapse = "")
  }
  
  custom_style <- paste(region, x_grid_line, y_grid_line, collapse = "")
  
  .bb_opt(bb, "customstyle", custom_style = custom_style)
}

