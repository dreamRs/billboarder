#' Helper for creating a bar chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param data A \code{data.frame}, the first column will be used for x axis unless
#' specified otherwise in \code{...}. If not a \code{data.frame}, an object coercible to \code{data.frame}.
#' @param stacked Logical, if several columns provided, produce a stacked bar chart, else
#' a dodge bar chart.
#' @param rotated Switch x and y axis position.
#' @param color Bar's color.
#' @param ... Arguments for slot bar, see \url{https://naver.github.io/billboard.js/release/latest/doc/Options.html#.bar}.
#' 
#' @note This function can be used with \code{\link{billboarderProxy}} in shiny application.
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
#' @examples
#' library("billboarder")
#' 
#' stars <- data.frame(
#'   package = c("billboarder", "ggiraph", "officer", "shinyWidgets", "visNetwork"),
#'   stars = c(1, 176, 42, 40, 166)
#' )
#' 
#' billboarder() %>%
#'   bb_barchart(data = stars)
#' 
#' billboarder() %>%
#'   bb_barchart(data = stars, labels = TRUE) %>%
#'   bb_data(names = list(stars = "Number of stars")) %>% 
#'   bb_axis(rotated = TRUE)
bb_barchart <- function(bb, data, stacked = FALSE, rotated = FALSE, color = NULL, ...) {
  
  if (missing(data))
    data <- bb$x$data
  
  data <- as.data.frame(data)
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
  
  
  data_opt <- list(
    x = x,
    json = json,
    type = "bar",
    groups = stacked
  )
  
  
  
  if ("billboarder_Proxy" %in% class(bb)) {
    
    # bb <- bb_load(proxy = bb, x = x, json = json, groups = stacked, unload = bb$unload) 
    
    if (!is.null(color)) {
      colp <- stats::setNames(as.list(color), setdiff(names(json), x))
    } else {
      colp <- NULL
    }
    
    bb <- bb_load(proxy = bb,
                  json = json, 
                  groups = stacked, 
                  x = x,
                  unload = bb$unload, 
                  colors = colp) 
    
    bb <- bb_categories(bb = bb, categories = json[[1]])

  } else {
    
    bb <- .bb_opt2(bb, "data", data_opt)
    
    bb <- .bb_opt(bb, "bar", ...)
    
    if (!is.null(color))
      bb <- bb_color(bb, color)
    
    bb <- .bb_opt(bb, "axis", x = list(type = "category"), rotated = rotated)
    
  }
  
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
#'  
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
#'   bb_barchart(data = mpg[, list(count = .N), by = manufacturer][order(count)]) %>%
#'   bb_bar_color_manual(values = cols)
#' }
bb_bar_color_manual <- function(bb, values) {
  
  x <- bb$x$bb_opts$data$x
  categories <- bb$x$bb_opts$data$json[[x]]
  
  if (is.null(categories))
    stop("This function must be called after 'bb_barchart'")
  
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



#' @title Set categories on X axis
#' 
#' @description Set or modify x axis labels.
#'
#' @param bb A \code{billboard} \code{htmlwidget} object. 
#' @param categories A character vector to set names on a category axis.
#' 
#' @note This function can be used with \code{\link{billboarder-shiny}} to modify labels on axis, e.g. for barcharts.
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
#' @examples
#' # Simple line with month names as x labels
#' billboarder() %>% 
#'   bb_linechart(data = round(rnorm(12))) %>% 
#'   bb_categories(categories = month.name)
#'   
bb_categories <- function(bb, categories) {
  
  if ("billboarder_Proxy" %in% class(bb)) {
    
    bb <- .bb_proxy(bb, "categories", categories)
    
  } else {

    bb <- .bb_opt(bb, "axis", x = list(type = "category", categories = categories))
    
  }
  
  bb
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
#' @note This function can be used with \code{\link{billboarderProxy}} in shiny application.
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#' 
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#' billboarder() %>% 
#'   bb_scatterplot(data = iris, x = "Sepal.Length", y = "Sepal.Width")
#' }
bb_scatterplot <- function(bb, data, x = NULL, y = NULL, group = NULL, ...) {
  
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
      object = as.list(paste(unique(data[[group]]), "x", sep = "_")), 
      nm = unique(data[[group]])
    )
    json <- c(
      split(x = data[[x]], f = paste(data[[group]], "x", sep = "_")),
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
  
  
  if ("billboarder_Proxy" %in% class(bb)) {
    
    bb <- bb_load(proxy = bb, json = json, xs = xs, unload = bb$unload) 
    
    bb <- bb_axis_labels(proxy = bb, x = x, y = y)
    
  } else {
    
    bb <- .bb_opt2(bb, "data", data_opt)
    
    bb <- .bb_opt(bb, "legend", show = !is.null(group))
    
    bb <- .bb_opt2(bb, "axis", data_axis)
    
  }
  
  
  return(bb)
}




#' Helper for creating a gauge
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param value A numeric value.
#' @param name Name for the value, appear in  tooltip.
#' @param steps Upper bound for changing colors
#' @param steps_color Colors corresponding to steps
#' @param ... Arguments for slot gauge.
#' 
#' @note This function can be used with \code{\link{billboarderProxy}} in shiny application.
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#' 
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#' billboarder() %>% 
#'  bb_gaugechart(value = 50)
#' }
bb_gaugechart <- function(bb, value, name = "Value", 
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
#' @param data A \code{data.frame}, first column must contain labels and second values associated.
#' @param ... Arguments for slot pie, \url{https://naver.github.io/billboard.js/release/latest/doc/Options.html#.pie}.
#' 
#' @note This function can be used with \code{\link{billboarderProxy}} in shiny application.
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
#'   bb_piechart(data = stars)
#' }
bb_piechart <- function(bb, data, ...) {
  
  if (missing(data))
    data <- bb$x$data
  
  data <- as.data.frame(data)
  
  json <- as.list(data[[2]])
  json <- lapply(X = json, FUN = list)
  names(json) <- data[[1]]
  
  data_opt <- list(
    json = json,
    type = "pie"
  )

  if ("billboarder_Proxy" %in% class(bb)) {
    
    bb <- bb_load(proxy = bb, json = json, unload = bb$unload) 
    
  } else {
    
    bb <- .bb_opt2(bb, "data", data_opt)
    
    bb <- .bb_opt(bb, "pie", ...)
    
  }
  
  return(bb)
}




#' Helper for creating a donut chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param data A \code{data.frame}.
#' @param ... Arguments for slot donut, \url{https://naver.github.io/billboard.js/release/latest/doc/Options.html#.donut}.
#' 
#' @note This function can be used with \code{\link{billboarderProxy}} in shiny application.
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
#'   bb_donutchart(data = stars, title = "Stars")
#' }
bb_donutchart <- function(bb, data, ...) {
  
  if (missing(data))
    data <- bb$x$data
  
  data <- as.data.frame(data)
  
  json <- as.list(data[[2]])
  json <- lapply(X = json, FUN = list)
  names(json) <- data[[1]]
  
  data_opt <- list(
    json = json,
    type = "donut"
  )
  
  if ("billboarder_Proxy" %in% class(bb)) {
    
    bb <- bb_load(proxy = bb, json = json, unload = bb$unload) 
    
  } else {
    
    bb <- .bb_opt2(bb, "data", data_opt)
    
    bb <- .bb_opt(bb, "donut", ...)
    
  }
  
  return(bb)
}







# Helper for creating an histogram
#
# @param bb A \code{billboard} \code{htmlwidget} object.
# @param x A numeric \code{vector}.
# @param breaks Arguments passed to \code{hist}.
# @param ... Arguments for slot 
#
# @return A \code{billboard} \code{htmlwidget} object.
# @export
# 
# @importFrom graphics hist
#
bb_histogram <- function(bb, x, breaks = "Sturges", ...) {
  
  
  h <- graphics::hist(x = x, breaks = breaks, plot = FALSE)
  
  json <- list(
    data = h$counts,
    x = h$breaks
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








#' Helper for creating a line chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param data A \code{data.frame} or a \code{vector}.
#' @param type Type of chart : line, spline, step, area, area-spline, area-step.
#' @param show_point Whether to show each point in line.
#' @param ... Not used.
#' 
#' @note This function can be used with \code{\link{billboarderProxy}} in shiny application.
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
#' @examples
#' 
#' # Different types
#' x <- round(rnorm(20), 2)
#' 
#' billboarder() %>% 
#'   bb_linechart(data = x)
#' 
#' billboarder() %>% 
#'   bb_linechart(data = x, type = "spline")
#' 
#' billboarder() %>% 
#'   bb_linechart(data = x, type = "area")
#' 
#' billboarder() %>% 
#'   bb_linechart(data = x, type = "area-spline")
#'   
#'   
#' # Timeserie with date (Date)
#' data("economics", package = "ggplot2")
#' 
#' billboarder() %>%
#'   bb_linechart(data = economics[, c("date", "psavert")]) %>% 
#'   bb_x_axis(tick = list(format = "%Y-%m", fit = FALSE)) %>%
#'   bb_y_axis(tick = list(format = suffix("%")), 
#'             label = list(text = "Personal savings rate")) %>% 
#'   bb_legend(show = FALSE) %>% 
#'   bb_x_grid(show = TRUE) %>% 
#'   bb_y_grid(show = TRUE) %>% 
#'   bb_subchart(show = TRUE)
#'   
#'
#' # Timeserie with datetime (POSIXct)
#' data("cdc_prod_filiere")
#' 
#' billboarder() %>% 
#'   bb_linechart(data = cdc_prod_filiere[, c("date_heure", "prod_eolien")])
#'  
#'  
#' ## Other type for x-axis 
#'  
#' # character/factor on x-axis
#' AirPassengers1960 <- data.frame(
#'   month = month.name, 
#'   AirPassengers = tail(AirPassengers, 12)
#' )
#' # you have to specify that x-axis is of type 'category'
#' billboarder() %>% 
#'   bb_linechart(data = AirPassengers1960, x = "month") %>% 
#'   bb_x_axis(type = "category")
#' 
#' 
#' # numeric on x-axis
#' lynx.df <- data.frame(
#'   year = time(lynx),
#'   lynx = lynx
#' )
#' # just specify which variable must be use n the x-axis
#' billboarder() %>% 
#'   bb_linechart(data = lynx.df, x = "year")
#'   
bb_linechart <- function(bb, data, type = "line", show_point = FALSE, ...) {
  
  type <- match.arg(
    arg = type, 
    choices = c("line", "spline", "step", "area", "area-spline", "area-step", "bar"),
    several.ok = TRUE
  )
  
  if (missing(data))
    data <- bb$x$data
  
  args <- list(...)
  
  if (is.vector(data)) {
    data_opt <- list(
      json = list(
        x = data
      ),
      type = type
    )
  } else {
    if (inherits(x = data[[1]], what = c("Date", "POSIXct"))) {
      if (inherits(x = data[[1]], what = c("POSIXct"))) {
        if (!"billboarder_Proxy" %in% class(bb)) {
          bb <- bb_data(bb, xFormat = "%Y-%m-%d %H:%M:%S")
        }
      }
      data[[1]] <- as.character(data[[1]])
      data_opt <- list(
        x = names(data)[1],
        json = as.list(data),
        type = type
      )
      if (!"billboarder_Proxy" %in% class(bb)) {
        bb <- bb_x_axis(bb, type = "timeseries")
      }
    } else {
      data_opt <- list(
        json = as.list(data),
        type = type
      )
    }
  }
  
  if ("billboarder_Proxy" %in% class(bb)) {
    
    bb <- bb_load(proxy = bb, json = data_opt$json, unload = bb$unload) 
    
  } else {
    
    bb <- .bb_opt2(bb, "data", c(data_opt, args))

    bb <- bb_point(bb, show = show_point)
    
  }
  
  return(bb)
}




#' Helper for creating a density plot
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param data A \code{data.frame} or a \code{vector}, the first column will
#'  be used to calculate density if \code{x} is \code{NULL}.
#' @param x The name of the variable to use in \code{data}.
#' @param group Variable to use to plot data by group.
#' @param stacked Logical, create a stacked density plot.
#' @param stat Stat to compute : \code{density} or \code{count}.
#' @param fill Produce a conditional density estimate, this option force \code{stacked = TRUE}.
#' @param ... Arguments passed to \code{\link[stats]{density}}.
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#' 
#' @seealso \code{\link{bb_histogram}}
#' 
#' @importFrom stats setNames density density.default
#'
#' @examples
#' data("diamonds", package = "ggplot2")
#' 
#' # density plot with one variable
#' billboarder() %>% 
#'   bb_densityplot(data = diamonds, x = "carat")
#' 
#' # With a grouping variable
#' billboarder() %>% 
#'   bb_densityplot(data = diamonds, x = "depth", group = "cut") %>% 
#'   bb_x_axis(min = 55, max = 70)
#' 
#' # a stacked density plot using count as statistic
#' bb <- billboarder() %>%
#'   bb_densityplot(data = diamonds, x = "depth", group = "cut",
#'                  stacked = TRUE, stat = "count") %>%
#'   bb_x_axis(min = 55, max = 70)
#' bb
#' 
#' # changing order
#' bb %>% bb_data(order = "asc")
bb_densityplot <- function(bb, data, x = NULL, group = NULL, stacked = FALSE, stat = "density", fill = FALSE, ...) {
  
  stat <- match.arg(arg = stat, choices = c("density", "count"))
  
  if (missing(data))
    data <- bb$x$data
  
  args <- list(...)
  argsdensity <- formals(density.default)
  bw <- args$bw %||% argsdensity$bw
  adjust <- args$adjust %||% argsdensity$adjust
  kernel <- args$kernel %||% "gaussian"
  weights <- args$weights %||% argsdensity$weights
  n <- args$n %||% argsdensity$n
  
  if (is.vector(data)) {
    data <- data.frame(x = data)
    x <- "x"
    if (!is.null(group)) {
      if (!(is.vector(group) | is.factor(group))) {
        stop("If 'data' is a vector, 'group' must be a vector of the same length if specified.")
      }
      data$group <- group
      group <- "group"
    }
  }
  
  x <- x %||% names(data)[1]
  
  # fill options
  ymax <- NULL
  ypadding <- NULL
  
  if (is.null(group)) {
    xs <- stats::setNames(list("x"), "y")
    json <- stats::density(
      x = data[[x]], bw = bw, adjust = adjust,
      kernel = kernel, weights = weights, n = n
    )[c("x", "y")]
    if (stat == "count") {
      nx <- length(data[[x]])
      json$y <- json$y * nx
    }
    groups <- NULL
  } else {
    data[[group]] <- as.character(data[[group]])
    groups <- unique(data[[group]])
    xs <- stats::setNames(as.list(paste0(groups, "_x")), groups)
    range_x <- range(data[[x]], na.rm = TRUE)
    json <- lapply(
      X = groups,
      FUN = function(g) {
        tmp <- data[[x]][data[[group]] %in% g]
        l <- stats::density(
          x = tmp, from = range_x[1], to = range_x[2], bw = bw, 
          adjust = adjust, kernel = kernel, weights = weights, n = n
        )[c("x", "y")]
        if (stat == "count") {
          nx <- length(tmp)
          l$y <- l$y * nx
        }
        stats::setNames(l, c(paste0(g, "_x"), g))
      }
    )
    if (fill) {
      max_y <- do.call(cbind, lapply(json, `[[`, 2))
      max_y <- apply(max_y, 1, sum, na.rm = TRUE)
      json <- lapply(json, function(x) {x[[2]] <- x[[2]] / max_y; x})
      ymax <- 1
      ypadding <- 0
    }
    json <- unlist(json, recursive = FALSE)
    if (stacked | fill) {
      groups <- list(groups)
    } else {
      groups <- NULL
    }
  }
  
  data_opt <- list(
    xs = xs,
    json = json,
    type = "area-spline",
    groups = groups
  )
  
  if (is.null(data_opt$groups)) {
    data_opt$groups <- NULL
  }
  
  data_axis <- list(
    x = list(
      label = list(
        text = x
      ),
      tick = list(
        fit = FALSE
      )
    ),
    y = list(
      max = ymax,
      padding = ypadding,
      label = list(
        text = stat
      )
    )
  )
  
  if ("billboarder_Proxy" %in% class(bb)) {
    
    bb <- bb_axis_labels(proxy = bb, x = x, y = stat)
    
    bb <- bb_load(proxy = bb, json = json, xs = xs, unload = bb$unload) 
    
  } else {
    
    bb <- .bb_opt2(bb, "data", data_opt)
    
    bb <- .bb_opt(bb, "legend", show = !is.null(group))
    
    bb <- .bb_opt2(bb, "axis", data_axis)
    
    bb <- bb_point(bb, show = FALSE)
  }
  
  return(bb)
}



#' Helper for creating an histogram
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param data A \code{data.frame} or a \code{vector}, the first column will
#'  be used to calculate density if \code{x} is \code{NULL}.
#' @param x The name of the variable to use in \code{data}.
#' @param group Variable to use to plot data by group.
#' @param stacked Logical, create a stacked histogram.
#' @param fill Logical, create a stacked percentage histogram.
#' @param bins Number of bins. Overridden by \code{binwidth}. Defaults to 30.
#' @param binwidth The width of the bins. See \code{\link[ggplot2]{geom_histogram}}
#' @param ... Not used.
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#' 
#' @seealso \code{\link{bb_densityplot}}
#' 
#' @importFrom stats reshape
#' @importFrom ggplot2 ggplot aes_string geom_histogram layer_data
#' @importFrom htmlwidgets JS
#'
#' @examples
#' data("diamonds", package = "ggplot2")
#' 
#' # one variable
#' billboarder() %>% 
#'   bb_histogram(data = diamonds, x = "price")
#' 
#' # equivalent to
#' billboarder() %>% 
#'   bb_histogram(data = diamonds$price)
#'   
#' # prettier with 'binwidth'
#' # (but you need to know your data)
#' billboarder() %>% 
#'   bb_histogram(data = diamonds, x = "price", binwidth = 500) %>% 
#'   bb_colors_manual()
#' 
#' # with a grouping variable
#' billboarder() %>%
#'   bb_histogram(data = diamonds, x = "price",
#'                group = "cut", binwidth = 500)
#' 
#' # stacked histogram
#' billboarder() %>%
#'   bb_histogram(data = diamonds, x = "price", group = "cut",
#'                stacked = TRUE, binwidth = 500)
#' 
#' 
#' # another example
#' dat <- data.frame(
#'   sample = c(rnorm(n = 500, mean = 1), rnorm(n = 500, mean = 2)),
#'   group = rep(c("A", "B"), each = 500)
#' )
#' 
#' billboarder() %>% 
#'   bb_histogram(data = dat, x = "sample", binwidth = 0.25)
#' 
#' samples_mean <- tapply(dat$sample, dat$group, mean)
#' billboarder() %>% 
#'   bb_histogram(data = dat, x = "sample", group = "group",
#'                binwidth = 0.25) %>% 
#'   bb_x_grid(
#'     lines = list(
#'       list(value = unname(samples_mean['A']),
#'            text = "mean of sample A"),
#'       list(value = unname(samples_mean['B']), 
#'            text = "mean of sample B")
#'     )
#'   )

bb_histogram <- function(bb, data, x = NULL, group = NULL, stacked = FALSE, fill = FALSE, bins = 30, binwidth = NULL, ...) {
  
  if (!requireNamespace(package = "ggplot2"))
    message("Package 'ggplot2' is required to run this function")
  
  if (missing(data))
    data <- bb$x$data
  
  args <- list(...)
  
  if (is.vector(data)) {
    data <- data.frame(x = data)
    x <- "x"
    if (!is.null(group)) {
      if (!(is.vector(group) | is.factor(group))) {
        stop("If 'data' is a vector, 'group' must be a vector of the same length if specified.")
      }
      data$group <- group
      group <- "group"
    }
  }
  
  x <- x %||% names(data)[1]
  
  # fill options
  ymax <- NULL
  ypadding <- NULL
  yticklabel <- NULL
  
  if (is.null(group)) {
    # compute data with ggplot
    p <- ggplot2::ggplot(data = data)
    p <- p + ggplot2::aes_string(x = x)
    p <- p + ggplot2::geom_histogram(bins = bins, binwidth = binwidth)
    
    # create json data
    dat <- ggplot2::layer_data(p, i = 1L)
    # dat$x <- round(dat$x)
    json <- as.list(dat[c("x", "y")])
    
    # grouping disabled
    groups <- NULL
    
  } else {
    # compute data with ggplot
    p <- ggplot2::ggplot(data = data)
    p <- p + ggplot2::aes_string(x = x, fill = group, text = group)
    p <- p + ggplot2::geom_histogram(bins = bins, binwidth = binwidth)
    
    # create json data
    dat <- ggplot2::layer_data(p, i = 1L)
    datr <- stats::reshape(data = dat[, c("x", "count", "text")], idvar = "x", timevar = "text", direction = "wide")
    names(datr) <- gsub(pattern = "count\\.", replacement = "", x = names(datr))
    if (fill) {
      vars <- names(datr)[-1]
      maxvars <- apply(datr[vars], 1, sum, na.rm = TRUE)
      datr[vars] <- lapply(datr[vars], function(x) round((x / maxvars) * 100, 2))
      # fill options
      ymax <- 100
      ypadding <- 0
      yticklabel <- suffix("%")
    }
    # sorting legend
    if (is.null(levels(data[[group]]))) {
      datr <- datr[sort(names(datr))]
    } else {
      datr <- datr[match(x = names(datr), table = c("x", levels(data[[group]])))]
    }
    datr$x <- round(datr$x, 3)
    json <- as.list(datr)
    
    if (stacked | fill) {
      groups <- list(as.character(unique(data[[group]])))
    } else {
      groups <- NULL
    }
  }
  
  data_opt <- list(
    x = "x",
    json = json,
    type = "area-step",
    groups = groups
  )
  
  if (is.null(data_opt$groups)) {
    data_opt$groups <- NULL
  }
  
  axis_opt <- list(
    x = list(
      label = list(
        text = x
      ),
      tick = list(
        fit = FALSE,
        outer = FALSE, 
        centered = TRUE
      )
    ),
    y = list(
      max = ymax,
      padding = ypadding,
      tick = list(format = yticklabel),
      label = list(
        text = "count"
      )
    )
  )
  
  
  
  # tooltip
  if (is.null(binwidth)) {
    binwidth <- dat$xmax[1] - dat$xmin[1]
    binwidth <- round(binwidth, 3)
  }
  tooltip_title <- paste0("var x = (i-", binwidth/2, ") + ' ; ' + (i+", binwidth/2, ");")
  tooltip_title <- paste("function(i) {", tooltip_title, "return x;", "}", collapse = "\n")
  tooltip_opt <- list(
    format = list(title = htmlwidgets::JS(tooltip_title))
  )
  
  if ("billboarder_Proxy" %in% class(bb)) {
    
    bb <- bb_axis_labels(proxy = bb, x = x, y = "count")
    
    bb <- bb_load(proxy = bb, json = json, x = x, unload = bb$unload) 
    
  } else {
    
    bb <- .bb_opt2(bb, "data", data_opt)
    
    bb <- .bb_opt(bb, "legend", show = !is.null(group))
    
    bb <- .bb_opt2(bb, "axis", axis_opt)
    
    bb <- .bb_opt2(bb, "tooltip", tooltip_opt)
    
  }
  
  return(bb)
}








#' Helper for creating a lollipop chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param data A \code{data.frame}, the first column will be used for x axis unless
#' argument \code{x} is speciefied, the second one will be use as y values.
#'  If not a \code{data.frame}, an object coercible to \code{data.frame}. 
#' @param x Character, the variable to use for the x axis.
#' @param y Character, the variable containing the values to map on the chart.
#' @param rotated Switch x and y axis position.
#' @param point_color Color of the lollipop.
#' @param point_size Size of the lollipop.
#' @param line_color Color of the lines between the axis and the lollipop.
#' @param ... 
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
#' @examples
#' # TODO
bb_lollipop <- function(bb, data, x = NULL, y = NULL, rotated = FALSE, point_color = "#112446", point_size = 8, line_color = "#000", ...) {
  
  if (missing(data))
    data <- bb$x$data
  
  data <- as.data.frame(data)
  args <- list(...)
  
  if (is.null(x))
    x <- names(data)[1]
  
  if (is.null(y))
    y <- names(data)[2]
  
  data <- data[c(x, y)]
  data <- cbind(data, lollipop = data[[y]])
  
  if (nrow(data) == 1) {
    json <- lapply(X = as.list(data), FUN = list)
  } else {
    json <- as.list(data)
  }
  
  data_opt <- list(
    x = x,
    json = json,
    type = "bar",
    classes = stats::setNames(list("lollipop-lines"), y),
    types = stats::setNames(c(list("bar", "line")), c("lollipop", y)),
    colors = stats::setNames(c(list(line_color, point_color)), c("lollipop", y))
  )
  
  bb <- .bb_opt2(bb, "data", data_opt)
  
  bb <- .bb_opt(bb, "axis", x = list(type = "category"), rotated = rotated)
  
  bb <- .bb_opt(bb, "bar", width = 0.05)
  
  bb <- .bb_opt(bb, "point", r = point_size)
  
  bb <- .bb_opt(bb, "legend", hide = "lollipop")
  
  bb <- bb_add_style(
    bb = bb, 
    ".bb-target-lollipop-lines > .bb-circle" = "opacity: 1;", 
    ".bb-target-lollipop-lines > .bb-lines" = "opacity: 0;"
  )
  
  bb <- .bb_opt(bb, "tooltip", format = list(
    value = htmlwidgets::JS("function(value, ratio, id, index) {if (id !== 'lollipop') return value; }")
  ))
  
  return(bb)
}
