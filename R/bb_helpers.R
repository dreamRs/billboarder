#' Helper for creating a bar chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param data A \code{data.frame}, the first column will be used for x axis unless
#' specified otherwise in \code{...}
#' @param stacked Logical, if several columns provided, produce a stacked bar chart, else
#' a dodge bar chart.
#' @param ... Arguments for slot data and bar
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
#' @examples
#' \dontrun{
#' stars <- data.frame(
#'   package = c("billboarder", "ggiraph", "officer", "shinyWidgets", "visNetwork"),
#'   stars = c(1, 176, 42, 40, 166)
#' )
#' billboarder() %>%
#'   bb_bar(data = stars)
#' 
#' billboarder() %>%
#'   bb_bar(data = stars, labels = TRUE, names = list(stars = "Number of stars")) %>%
#'   bb_axis(rotated = TRUE)
#' }
bb_bar <- function(bb, data, stacked = FALSE, ...) {
  
  if (missing(data))
    data <- bb$x$data
  
  args <- list(...)
  
  x <- args$x %||% names(data)[1]
  
  if (stacked) {
    stacked <- list(as.list(setdiff(names(data), x)))
  } else {
    stacked <- list()
  }
  
  data_names <- setdiff(names(args), c("width", "zerobased"))
  data_opt <- list(
    x = x,
    json = as.list(data),
    type = "bar",
    groups = stacked
  )
  data_opt <- c(data_opt, args[names(args) %in% data_names])
  bb <- .bb_opt2(bb, "data", data_opt)
  
  bb <- .bb_opt2(bb, "bar", args[names(args) %in% c("width", "zerobased")])
  
  bb <- .bb_opt(bb, "axis", x = list(type = "category"))
  
  return(bb)
}








#' Helper for creating a scatter chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param data A \code{data.frame}
#' @param x Variable to map to the x-axis, if \code{NULL} first variable is used.
#' @param y Variable to map to the y-axis, if \code{NULL} second variable is used.
#' @param group Variable to use to plot data by group.
#' @param ... unused
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#' 
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#' billboarder() %>% 
#'   bb_points(data = iris, x = "Sepal.Length", y = "Sepal.Width")
#' }
bb_points <- function(bb, data, x = NULL, y = NULL, group = NULL, ...) {
  
  if (missing(data))
    data <- bb$x$data
  
  args <- list(...)
  
  x <- x %||% names(data)[1]
  y <- y %||% names(data)[2]
  
  if (is.null(group)) {
    xs <- setNames(list(x), y)
    json <- as.list(data[, c(x, y)])
  } else {
    xs <- setNames(
      object = as.list(paste(unique(data[[group]]), x, sep = "_")), 
      nm = unique(data[[group]])
    )
    json <- c(
      split(x = data[[x]], f = paste(data[[group]], x, sep = "_")),
      split(x = data[[y]], f = data[[group]])
    )
  }
  
  data_opt <- list(
    xs = xs,
    json = json,
    type = "scatter"
  )
  
  data_axis <- list(
    x = list(
      label = list(
        text = x
      )
    ),
    y = list(
      label = list(
        text = y
      )
    )
  )
  
  bb <- .bb_opt2(bb, "data", data_opt)
  
  bb <- .bb_opt(bb, "legend", show = !is.null(group))
  
  bb <- .bb_opt2(bb, "axis", data_axis)
  
  return(bb)
}





