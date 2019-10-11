#' Add custom style for regions and grid lines
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param region A named list with style associated with region.
#' @param x_grid A named list with style associated with grid line on the X-axis.
#' @param y_grid A named list with style associated with grid line on the Y-axis.
#' @param ...,.list  Used internally.
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
#' # Customize grid line and text
#' billboarder() %>% 
#'   bb_linechart(data = sin(seq(-pi, pi, length.out = 30))) %>% 
#'   bb_y_grid(lines = list(list(
#'     value = 0, text = "Zero", position  = "middle", class = "zero"
#'   ))) %>% 
#'   bb_add_style(y_grid = list(
#'     zero = list(line = "stroke: red", text = "font-size: 240%; fill: black"
#'   )))
#' 
bb_add_style <- function(bb, region = NULL, x_grid = NULL, y_grid = NULL, ..., .list = NULL) {
  
  if (!is.null(region)) {
    region <- paste0(".bb-region.", names(region), "{", unlist(region, use.names = FALSE), "}")
    region <- paste(region, collapse = "")
  }
  if (!is.null(x_grid)) {
    x_grid_tmp <- lapply(
      X = x_grid,
      FUN = function(x) {
        paste0(names(x), "{", unlist(x, use.names = FALSE), "}")
      }
    )
    x_grid_tmp <- paste0(".bb-xgrid-line.", names(x_grid_tmp), " ", unlist(x_grid_tmp, use.names = FALSE))
    x_grid <- paste(x_grid_tmp, collapse = " ")
  }
  if (!is.null(y_grid)) {
    y_grid_tmp <- lapply(
      X = y_grid,
      FUN = function(x) {
        paste0(names(x), "{", unlist(x, use.names = FALSE), "}")
      }
    )
    y_grid_tmp <- paste0(".bb-ygrid-line.", names(y_grid_tmp), " ", unlist(y_grid_tmp, use.names = FALSE))
    y_grid <- paste(y_grid_tmp, collapse = " ")
  }
  
  args <- c(list(...), .list)
  if (length(args) > 0) {
    args <- paste0(names(args), "{", unlist(args, use.names = FALSE), "}")
    # args <- paste(args, collapse = "")
  }
  
  custom_style <- c(region, x_grid, y_grid, args)
  
  # .bb_opt(bb, "customstyle", custom_style = custom_style)
  bb$x$bb_opts$customStyle <- c(bb$x$bb_opts$customStyle, custom_style)
  bb
}

