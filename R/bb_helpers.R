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
  
  if (is.null(data)) {
    bb <- .bb_opt(bb, "bar", ...)
    return(bb)
  }
  
  x <- args$x %||% names(data)[1]
  
  if (stacked) {
    stacked <- list(as.list(base::setdiff(names(data), x)))
  } else {
    stacked <- list()
  }
  
  if (nrow(data) == 1) {
    json <- lapply(X = as.list(data), FUN = list)
  } else {
    json <- as.list(data)
  }
  
  data_names <- base::setdiff(names(args), c("width", "zerobased"))
  data_opt <- list(
    x = x,
    json = json,
    type = "bar",
    groups = stacked
  )
  data_opt <- c(data_opt, args[names(args) %in% data_names])
  bb <- .bb_opt2(bb, "data", data_opt)
  
  bb <- .bb_opt2(bb, "bar", args[names(args) %in% c("width", "zerobased")])
  
  bb <- .bb_opt(bb, "axis", x = list(type = "category"))
  
  return(bb)
}



#' Manual color for barchart
#' 
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param values A named vector, names represent the categories of the bar chart,
#' values correspond to colors. All categories must be present in the vector, in 
#' the same order of the chart.
#' 
#' @note Must be called after \code{bb_bar}.
#' #' 
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#' 
#' @importFrom jsonlite toJSON
#' @importFrom htmlwidgets JS
#' 
#' @examples
#' \dontrun{
#' 
#' library("data.table")
#' library("billboarder")
#' 
#' data("mpg", package = "ggplot2")
#' setDT(mpg)
#' 
#' # all in blue
#' manufa <- unique(mpg$manufacturer)
#' cols <- rep("#08298A", length(manufa))
#' names(cols) <- manufa
#' 
#' # Nissan in red
#' cols[["nissan"]] <- "#DF0101"#' 
#' 
#' billboarder() %>%
#'   bb_bar(data = mpg[, list(count = .N), by = manufacturer][order(count)]) %>%
#'   bb_bar_color_manual(values = cols)
#' }
bb_bar_color_manual <- function(bb, values) {
  
  x <- bb$x$bb_opts$data$x
  categories <- bb$x$bb_opts$data$json[[x]]
  
  if (is.null(categories))
    stop("This function must be called after 'bb_bar'")
  
  colorjs <- htmlwidgets::JS(
    paste(
      "function(color, d) {",
      paste0(
        "var x = ", jsonlite::toJSON(categories), "; "
      ),
      paste(
        sprintf(
          "if (x[d.index] == '%s') { return '%s'; }", 
          names(values), unname(values)
        ),
        collapse = "\n"
      ),
      "else { return color; }",
      "}", collapse = "\n"
    )
  )
  
  bb <- .bb_opt(bb, "data", color = colorjs)
  
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
#'   bb_scatter(data = iris, x = "Sepal.Length", y = "Sepal.Width")
#' }
bb_scatter <- function(bb, data, x = NULL, y = NULL, group = NULL, ...) {
  
  if (missing(data))
    data <- bb$x$data
  
  args <- list(...)
  
  x <- x %||% names(data)[1]
  y <- y %||% names(data)[2]
  
  if (is.null(group)) {
    xs <- stats::setNames(list(x), y)
    json <- as.list(data[, c(x, y)])
  } else {
    xs <- stats::setNames(
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




#' Helper for creating a gauge
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param value A numeric value.
#' @param name Name for the value, appear in  tooltip.
#' @param steps Upper bound for changing colors
#' @param steps_color Colors corresponding to steps
#' @param ... Arguments for slot gauge
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#' 
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#' billboarder() %>% 
#'  bb_gauge(value = 50)
#' }
bb_gauge <- function(bb, value, name = "Value", 
                     steps = c(30, 60, 90, 100),
                     steps_color = c("#FF0000", "#F97600", "#F6C600", "#60B044"),
                     ...) {
  
  if (missing(value) || is.null(value)) {
    bb <- .bb_opt(bb, "gauge", ...)
    return(bb)
  }
  
  if (length(steps) != length(steps_color))
    stop("'steps' and 'steps_color' must have same length.")
  
  data_opt <- list(
    json = stats::setNames(list(list(value)), name),
    type = "gauge"
  )
  
  data_color <- list(
    pattern = steps_color,
    threshold = list(values = steps)
  )
  
  
  if ("billboarder" %in% class(bb)) {
    
    bb <- .bb_opt2(bb, "data", data_opt)
    
    bb <- .bb_opt(bb, "gauge", ...)
    
    bb <- .bb_opt2(bb, "color", data_color)
    
    return(bb)
    
  } else if ("billboarder_Proxy" %in% class(bb)) {
    .bb_proxy(bb, "data", json = data_opt$json, ...)
  }
  
}




#' Helper for creating a pie chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param data A \code{data.frame}.
#' @param ... Arguments for slot pie, \url{https://naver.github.io/billboard.js/release/latest/doc/Options.html#.pie}.
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#' 
#' @examples
#' \dontrun{
#' stars <- data.frame(
#'   package = c("billboarder", "ggiraph", "officer", "shinyWidgets", "visNetwork"),
#'   stars = c(9, 177, 43, 44, 169)
#' )
#' 
#' billboarder() %>% 
#'   bb_pie(data = stars)
#' }
bb_pie <- function(bb, data, ...) {
  
  if (missing(data))
    data <- bb$x$data
  
  json <- as.list(data[[2]])
  json <- lapply(X = json, FUN = list)
  names(json) <- data[[1]]
  
  data_opt <- list(
    json = json,
    type = "pie"
  )

  bb <- .bb_opt2(bb, "data", data_opt)
  
  bb <- .bb_opt(bb, "pie", ...)
  
  return(bb)
}




#' Helper for creating a donut chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param data A \code{data.frame}.
#' @param ... Arguments for slot donut, \url{https://naver.github.io/billboard.js/release/latest/doc/Options.html#.donut}.
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#' 
#' @examples
#' \dontrun{
#' stars <- data.frame(
#'   package = c("billboarder", "ggiraph", "officer", "shinyWidgets", "visNetwork"),
#'   stars = c(9, 177, 43, 44, 169)
#' )
#' 
#' billboarder() %>% 
#'   bb_donut(data = stars, title = "Stars")
#' }
bb_donut <- function(bb, data, ...) {
  
  if (missing(data))
    data <- bb$x$data
  
  json <- as.list(data[[2]])
  json <- lapply(X = json, FUN = list)
  names(json) <- data[[1]]
  
  data_opt <- list(
    json = json,
    type = "donut"
  )
  
  bb <- .bb_opt2(bb, "data", data_opt)
  
  bb <- .bb_opt(bb, "donut", ...)
  
  return(bb)
}







#' Helper for creating an histogram
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param data A \code{data.frame}.
#' @param ... Arguments for slot 
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#' 
#' @importFrom graphics hist
#' 
#' @examples
#' \dontrun{
#' }
bb_hist <- function(bb, x, breaks = "Sturges", ...) {
  
  
  h <- graphics::hist(x = x, breaks = breaks, plot = FALSE)
  
  json <- list(
    data = h$counts,
    x = head(h$breaks, -1)
  )
  
  
  data_opt <- list(
    json = json,
    type = "area-step",
    x = "x"
  )
  
  bb <- .bb_opt2(bb, "data", data_opt)
  
  bb <- .bb_opt(bb, "area", ...)
  
  bb <- .bb_opt(bb, "line", step = list(type = "step"))
  
  return(bb)
}

