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