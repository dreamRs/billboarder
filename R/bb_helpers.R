#' Helper for creating a bar chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param data A \code{data.frame}, the first column will be used for x axis unless
#' specified otherwise in \code{mapping}. If not a \code{data.frame}, an object coercible to \code{data.frame}.
#' @param mapping Mapping of variables on the chart, see \code{\link{bbaes}}.
#' @param stacked Logical, if several columns are provided, produce a stacked bar chart, else
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
#' 
#' stars <- data.frame(
#'   package = c("billboarder", "ggiraph", "officer",
#'               "shinyWidgets", "visNetwork", "rAmCharts", 
#'               "D3partitionR"),
#'   stars = c(67, 252, 160, 144, 224, 32, 25)
#' )
#' 
#' # By default, first column is mapped on the x-axis
#' # second one on the y axis
#' billboarder() %>%
#'   bb_barchart(data = stars)
#' 
#' 
#' # Specify explicitly the columns to use
#' billboarder() %>%
#'   bb_barchart(data = stars, mapping = bbaes(package, stars), rotated = TRUE)
#' 
#' 
#' # Add some options
#' billboarder() %>%
#'   bb_barchart(data = stars[order(stars$stars), ], x = "package", y = "stars", rotated = TRUE) %>% 
#'   bb_data(names = list(stars = "Number of stars")) %>% 
#'   bb_y_grid(show = TRUE)
#' 
#' 
#' 
#' # Hack stacked barcharts (to color bar)
#' stars_wide <- data.frame(
#'   author = c("dreamRs", "davidgohel", "davidgohel", "dreamRs",
#'              "datastorm-open", "datastorm-open", "AntoineGuillot2"),
#'   package = c("billboarder", "ggiraph", "officer",
#'               "shinyWidgets", "visNetwork", "rAmCharts", 
#'               "D3partitionR"),
#'   stars = c(67, 252, 160, 144, 224, 32, 25)
#' )
#' 
#' billboarder() %>%
#'   bb_barchart(data = stars_wide, 
#'               mapping = bbaes(package, stars, group = author),
#'               stacked = TRUE)
#' 
#' billboarder() %>%
#'   bb_barchart(data = stars_wide,
#'               mapping = bbaes(author, stars, group = package),
#'               stacked = TRUE)
#' 
#' 
#' 
#' # Grouping variable
#' tab <- table(sample(letters[1:5], 100, TRUE), sample(LETTERS[1:5], 100, TRUE))
#' dat <- as.data.frame(tab)
#' 
#' billboarder() %>%
#'   bb_barchart(data = dat, bbaes(x = Var1, y = Freq, group = Var2), rotated = TRUE)
#' 
#' 
#' # You can also pass data in a 'wide' format
#' dat2 <- data.frame(
#'   x = letters[1:5],
#'   A = sample.int(n = 100, size = 5),
#'   B = sample.int(n = 100, size = 5),
#'   C = sample.int(n = 100, size = 5),
#'   D = sample.int(n = 100, size = 5),
#'   E = sample.int(n = 100, size = 5)
#' )
#' 
#' # But cannot use mapping
#' billboarder() %>%
#'   bb_barchart(data = dat2, stacked = TRUE) %>% 
#'   bb_data(order = NULL, labels = TRUE)

bb_barchart <- function(bb, data, mapping = NULL, stacked = FALSE, rotated = FALSE, color = NULL, ...) {
  
  if (missing(data))
    data <- bb$x$data
  
  data <- as.data.frame(data)
  args <- list(...)
  
  mapping <- mapping %||% bb$x$mapping
  
  
  if (is.null(mapping)) {
    
    x <- names(data)[1]
    y <- names(data)[-1]
    
    if (stacked) {
      stacked <- list(as.list(y))
    } else {
      stacked <- NULL
    }
    
    if (nrow(data) == 1) {
      json <- lapply(X = as.list(data[c(x, y)]), FUN = list)
    } else {
      json <- as.list(data[c(x, y)])
    }
    names(json)[which(names(json) == x)] <- getOption("billboarder-x", default = "bb-x")
    data_opt <- list(
      x = getOption("billboarder-x", default = "bb-x"),
      json = json,
      type = "bar",
      groups = stacked
    )
    
  } else {
    
    json <- bbmapping(data = data, mapping = mapping)
    x <- as_label(mapping$x)
    names(json)[which(names(json) == x)] <- getOption("billboarder-x", default = "bb-x")
    
    if (is.null(mapping$group)) {
      stacked <- NULL
    } else {
      if (stacked) {
        stacked <- list(setdiff(names(json), getOption("billboarder-x", default = "bb-x")))
      } else {
        stacked <- NULL
      }
    }
    
    data_opt <- list(
      x = getOption("billboarder-x", default = "bb-x"),
      json = json,
      type = "bar",
      groups = stacked
    )
    
  }
  
  if ("billboarder_Proxy" %in% class(bb)) {
    
    # bb <- bb_load(proxy = bb, x = x, json = json, groups = stacked, unload = bb$unload) 
    
    if (!is.null(color)) {
      colp <- stats::setNames(as.list(color), setdiff(names(json), getOption("billboarder-x", default = "bb-x")))
    } else {
      colp <- NULL
    }
    
    bb <- bb_load(proxy = bb,
                  json = json, 
                  groups = stacked, 
                  x = getOption("billboarder-x", default = "bb-x"),
                  unload = bb$unload, 
                  colors = colp) 
    
    bb <- bb_categories(bb = bb, categories = json[[1]])

  } else {
    
    bb <- .bb_opt2(bb, "data", dropNulls(data_opt))
    
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
#' @param mapping Mapping of variables on the chart, see \code{\link{bbaes}}.
#' @param ... Alternative mapping, you can specify \code{x = "Sepal.Length"} for example.
#' @param point_opacity Opacity for points, value between \code{[0,1]}.
#' 
#' @note This function can be used with \code{\link{billboarderProxy}} in shiny application.
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#' 
#' @importFrom stats setNames
#' @importFrom scales rescale
#'
#' @examples
#' # Use first and second variable by default
#' billboarder() %>% 
#'   bb_scatterplot(data = iris)
#' 
#' 
#' # Explicit mapping
#' billboarder() %>% 
#'   bb_scatterplot(
#'     data = iris, 
#'     mapping = bbaes(Petal.Length, Petal.Width)
#'   ) %>% 
#'   bb_x_axis(tick = list(fit = FALSE))
#' 
#' 
#' # Grouping variable
#' billboarder() %>% 
#'   bb_scatterplot(
#'     data = iris, 
#'     mapping = bbaes(Sepal.Length, Sepal.Width, group = Species)
#'   )
#'   
#' # Size variable
#' billboarder() %>% 
#'   bb_scatterplot(
#'     data = iris, 
#'     mapping = bbaes(
#'       Sepal.Length, Sepal.Width,
#'       group = Species, size = Petal.Width
#'     )
#'   ) %>% 
#'   bb_x_axis(tick = list(fit = FALSE))
#'
bb_scatterplot <- function(bb, data, mapping = NULL, ..., point_opacity = NULL) {
  
  if (missing(data))
    data <- bb$x$data
  
  args <- list(...)
  mapping <- mapping %||% bb$x$mapping
  
  if (is.null(mapping)) {
    x <- args$x %||% names(data)[1]
    y <- args$y %||% names(data)[2]
    z <- args$z
    group <- args$group
  } else {
    data <- lapply(mapping, rlang::eval_tidy, data = data)
    nms <- lapply(mapping, rlang::as_label)
    names(data) <- unlist(nms, use.names = FALSE)
    x <- nms$x
    y <- nms$y
    if (!is.null(mapping$group)) {
      group <- nms$group
    } else {
      group <- NULL
    }
    if (!is.null(mapping$size)) {
      z <- nms$size
    } else {
      z <- NULL
    }
  }
  
  if (is.null(group)) {
    xs <- stats::setNames(list(x), y)
    json <- data[c(x, y)]
    if (!is.null(z)) {
      json$y <- mapply(
        FUN = function(y, z) list(y = y, z = z), 
        y = json$y, z = data[[z]], 
        SIMPLIFY = FALSE
      )
    }
  } else {
    xs <- stats::setNames(
      object = as.list(paste(unique(data[[group]]), "x", sep = "_")), 
      nm = unique(data[[group]])
    )
    if (!is.null(z)) {
      json <- c(
        split(x = data[[x]], f = paste(data[[group]], "x", sep = "_")),
        split(
          x = mapply(
            FUN = function(y, z) list(y = y, z = z), 
            y = data[[y]], z = data[[z]], 
            SIMPLIFY = FALSE
          ), 
          f = data[[group]]
        )
      )
    } else {
      json <- c(
        split(x = data[[x]], f = paste(data[[group]], "x", sep = "_")),
        split(x = data[[y]], f = data[[group]])
      )
    }
  }
  
  data_opt <- list(
    xs = xs,
    json = lapply(json, as.list),
    type = if (is.null(z)) "scatter" else "bubble"
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
    bb <- bb_proxy_axis_labels(proxy = bb, x = x, y = y)
  } else {
    bb <- .bb_opt2(bb, "data", data_opt)
    bb <- .bb_opt(bb, "legend", show = !is.null(group))
    bb <- .bb_opt2(bb, "axis", data_axis)
    if (!is.null(z)) {
      bb <- .bb_opt2(bb, "axis", list(x = list(padding = list(right = 0.1))))
    }
    if (!is.null(point_opacity)) {
      bb <- bb_add_style(bb, ".bb-circles circle" = sprintf("opacity: %s !important;", point_opacity))
    }
  }
  
  return(bb)
}




#' Helper for creating a gauge
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param value A single numeric value or a vector for stacked gauge.
#' @param name Name for the value, appear in  tooltip, same length as `value`.
#' @param color Color for the gauge, if provided, `steps` and `steps_color` are ignored.
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
#' billboarder() %>% 
#'   bb_gaugechart(value = 50)
#' 
#' # With some options
#' billboarder() %>% 
#'   bb_gaugechart(
#'     value = 160,
#'     steps_color = rev(c("#FF0000", "#F97600", "#F6C600", "#60B044"))
#'   ) %>% 
#'   bb_gauge(
#'     label = list(format = suffix("km/h")),
#'     min = 10, max = 200, width = 20
#'   )
#'
bb_gaugechart <- function(bb,
                          value,
                          name = "Value", 
                          color = NULL,
                          steps = c(30, 60, 90, 100),
                          steps_color = c("#FF0000", "#F97600", "#F6C600", "#60B044"),
                          ...) {
  
  if (missing(value) || is.null(value)) {
    bb <- .bb_opt(bb, "gauge", ...)
    return(bb)
  }
  
  if (length(value) == 1) {
    data_opt <- list(
      json = stats::setNames(list(list(value)), name[1]),
      type = "gauge"
    )
  } else {
    data_opt <- list(
      json = stats::setNames(lapply(value, as.list), name),
      type = "gauge"
    )
  }
  
  if (is.null(color)) {
    if (length(steps) != length(steps_color))
      stop("'steps' and 'steps_color' must have same length.")
    data_color <- list(
      pattern = steps_color,
      threshold = list(values = steps)
    )
  } else {
    data_color <- list(
      pattern = list1(color)
    )
  }
  
  
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
#' @param data A \code{data.frame}, first column should contain labels, second column values associated, except if mapping is provided.
#' @param mapping Mapping of variables on the chart, see \code{\link{bbaes}}.
#' @param ... Arguments for slot pie, \url{https://naver.github.io/billboard.js/release/latest/doc/Options.html#.pie}.
#' 
#' @note This function can be used with \code{\link{billboarderProxy}} in shiny application.
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#' 
#' @importFrom stats setNames
#' 
#' @examples
#' 
#' stars <- data.frame(
#'   package = c("billboarder", "ggiraph", "officer", "shinyWidgets", "visNetwork"),
#'   stars = c(9, 177, 43, 44, 169)
#' )
#' 
#' # Default
#' billboarder() %>% 
#'   bb_piechart(data = stars)
#' 
#' # Explicit mapping
#' billboarder() %>% 
#'   bb_piechart(data = stars, bbaes(package, stars))
#' 
#' # Other way to specify mapping
#' billboarder(data = stars) %>% 
#'   bb_aes(package, stars) %>% 
#'   bb_piechart()
#'
bb_piechart <- function(bb, data, mapping = NULL, ...) {
  
  if (missing(data))
    data <- bb$x$data
  
  data <- as.data.frame(data)
  
  mapping <- mapping %||% bb$x$mapping
  
  if (is.null(mapping)) {
    
    json <- as.list(data[[2]])
    json <- lapply(X = json, FUN = list)
    names(json) <- data[[1]]
    
  } else {
    
    if (!is.null(mapping$group)) {
      message("'group' isn't used for pie charts.")
      mapping$group <- NULL
    }
      
    
    data_mapped <- bbmapping(data = data, mapping = mapping)
    
    json <- mapply(
      FUN = function(name, value) {stats::setNames(list(list(value)), name)}, 
      name = as.factor(data_mapped[[1]]), value = data_mapped[[2]]
    )
    
  }
  
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
#' @param mapping Mapping of variables on the chart, see \code{\link{bbaes}}.
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
bb_donutchart <- function(bb, data, mapping = NULL, ...) {
  
  if (missing(data))
    data <- bb$x$data
  
  data <- as.data.frame(data)
  mapping <- mapping %||% bb$x$mapping
  
  if (is.null(mapping)) {
    
    json <- as.list(data[[2]])
    json <- lapply(X = json, FUN = list)
    names(json) <- data[[1]]
    
  } else {
    
    if (!is.null(mapping$group)) {
      message("'group' isn't used for donut charts.")
      mapping$group <- NULL
    }
      
    
    data_mapped <- bbmapping(data = data, mapping = mapping)
    
    json <- mapply(
      FUN = function(name, value) {stats::setNames(list(list(value)), name)}, 
      name = as.factor(data_mapped[[1]]), value = data_mapped[[2]]
    )
    
  }
  
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






#' Helper for creating a line chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param data A \code{data.frame} or a \code{vector}.
#' @param mapping Mapping of variables on the chart, see \code{\link{bbaes}}.
#' @param type Type of chart : \code{"line"}, \code{"spline"}, \code{"step"}, \code{"area"}, \code{"area-spline"}, \code{"area-step"}, 
#'  \code{"area-line-range"}, \code{"area-spline-range"}.
#' @param show_point Whether to show each point in line.
#' @param dasharray Pattern of dashes and gaps used to paint the outline of the line,
#'  see \url{https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stroke-dasharray} for specifications.
#' @param width Width of the stroke to be applied to the line,
#'  see \url{https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stroke-width} for specifications.
#' @param ... Not used.
#' 
#' @note Types area-line-range and area-spline-range don't work in RStudio viewer, open chart in a browser. 
#'  This function can be used with \code{\link{billboarderProxy}} in shiny application.
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
#' @examples
#' 
#' ## Different types
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
#' ## Timeserie with date (Date)
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
#' # With multiple lines :
#' 
#' data("economics_long", package = "ggplot2")
#' 
#' billboarder() %>%
#'   bb_linechart(economics_long, bbaes(date, value, variable)) %>% 
#'   bb_data(hide = "pop") %>% 
#'   bb_x_axis(tick = list(format = "%Y-%m", fit = FALSE))
#'   
#'   
#'
#' ## Timeserie with datetime (POSIXct)
#' data("cdc_prod_filiere")
#' 
#' billboarder() %>% 
#'   bb_linechart(data = cdc_prod_filiere[, c("date_heure", "prod_eolien")])
#' 
#' # or with mapping :
#' billboarder() %>% 
#'   bb_linechart(cdc_prod_filiere, bbaes(date_heure, prod_bioenergies))
#'   
#'   
#'  
#' ### Other type for x-axis 
#'  
#' ## character/factor on x-axis
#' AirPassengers1960 <- data.frame(
#'   month = month.name, 
#'   AirPassengers = tail(AirPassengers, 12)
#' )
#' # you have to specify that x-axis is of type 'category'
#' # and that column 'month' must be used for x-axis values
#' billboarder() %>% 
#'   bb_linechart(data = AirPassengers1960, x = "month") %>% 
#'   bb_x_axis(type = "category")
#' 
#' 
#' ## numeric on x-axis
#' lynx.df <- data.frame(
#'   year = time(lynx),
#'   lynx = lynx
#' )
#' # just specify which variable must be use n the x-axis
#' billboarder() %>% 
#'   bb_linechart(data = lynx.df, x = "year")
#'   
#'   
#' ### Area range charts
#' 
#' # Generate data
#' dat <- data.frame(
#'   date = seq.Date(Sys.Date(), length.out = 20, by = "day"),
#'   y1 = round(rnorm(20, 100, 15)),
#'   y2 = round(rnorm(20, 100, 15))
#' )
#' dat$ymin1 <- dat$y1 - 5
#' dat$ymax1 <- dat$y1 + 5
#' 
#' dat$ymin2 <- dat$y2 - sample(3:15, 20, TRUE)
#' dat$ymax2 <- dat$y2 + sample(3:15, 20, TRUE)
#' 
#' 
#' # Make chart : use ymin & ymax aes for range
#' billboarder(data = dat) %>% 
#'   bb_linechart(
#'     mapping = bbaes(x = date, y = y1, ymin = ymin1, ymax = ymax1),
#'     type = "area-line-range"
#'   ) %>% 
#'   bb_linechart(
#'     mapping = bbaes(x = date, y = y2, ymin = ymin2, ymax = ymax2), 
#'     type = "area-spline-range"
#'   ) %>% 
#'   bb_y_axis(min = 50)
#'   
bb_linechart <- function(bb, data, mapping = NULL, type = "line", 
                         show_point = FALSE, dasharray = NULL, width = NULL,
                         ...) {
  
  type <- match.arg(
    arg = type, 
    choices = c("line", "spline", "step", "area", 
                "area-spline", "area-step", "bar",
                "area-line-range", "area-spline-range"),
    several.ok = TRUE
  )
  
  if (missing(data))
    data <- bb$x$data
  
  mapping <- mapping %||% bb$x$mapping
  
  args <- list(...)
  
  if (is.vector(data)) {
    if (!is.null(mapping))
      warning("'mapping' is ignored when 'data' is a vector.")
    data <- data.frame(
      index = seq_along(data),
      x = data
    )
  }
  
  if (!is.null(mapping)) {
    data <- bbmapping(data = data, mapping = mapping)
    # if (!is.null(bb$data$json$y))
  } 
  if (inherits(x = data[[1]], what = c("Date", "POSIXct"))) {
    if (inherits(x = data[[1]], what = c("POSIXct"))) {
      if (!"billboarder_Proxy" %in% class(bb)) {
        bb <- bb_data(bb, xFormat = "%Y-%m-%d %H:%M:%S")
      }
    }
    if (inherits(x = data[[1]], what = c("POSIXct"))) {
      data[[1]] <- format(data[[1]], format = "%Y-%m-%d %H:%M:%S")
    } else if (inherits(x = data[[1]], what = c("Date"))) {
      data[[1]] <- format(data[[1]], format = "%Y-%m-%d")
    } else {
      data[[1]] <- as.character(data[[1]])
    }
    if (!"billboarder_Proxy" %in% class(bb)) {
      bb <- bb_x_axis(bb, type = "timeseries")
    }
  }
  if (length(data) > 1) {
    data_opt <- list(
      x = names(data)[1],
      json = as.list(data),
      types = setNames(as.list(rep_len(type, length(data) - 1)), nm = names(data)[-1])
    )
  } else {
    data_opt <- list(
      json = as.list(data),
      types = setNames(list(type), nm = names(data)[1])
    )
  }
  
  if ("billboarder_Proxy" %in% class(bb)) {
    
    bb <- bb_load(proxy = bb, json = data_opt$json, unload = bb$unload) 
    
  } else {
    
    if (!is.null(dasharray)) {
      dasharray <- setNames(
        object = as.list(sprintf("stroke-dasharray:%s;", rep_len(dasharray, length(data) - 1))), 
        nm = sprintf(".billboarder-line-%s", names(data)[-1])
      )
      bb <- bb_add_style(bb, .list = dasharray)
    }
    
    if (!is.null(width)) {
      width <- setNames(
        object = as.list(sprintf("stroke-width:%s;", rep_len(width, length(data) - 1))), 
        nm = sprintf(".billboarder-line-%s", names(data)[-1])
      )
      bb <- bb_add_style(bb, .list = width)
    }
    
    bb <- .bb_opt(bb, "line", classes = as.list(sprintf("billboarder-line-%s", names(data)[-1])))
    
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
#' @param mapping Mapping of variables on the chart, see \code{\link{bbaes}}.
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
#' 
#' # With a vector
#' billboarder() %>%
#'   bb_densityplot(data = rnorm(1e4))
#' 
#' data("diamonds", package = "ggplot2")
#' 
#' # density plot with one variable
#' billboarder() %>% 
#'   bb_densityplot(data = diamonds, x = "carat")
#' 
#' # Same with mapping
#' billboarder() %>% 
#'   bb_densityplot(diamonds, bbaes(carat))
#' 
#' # With a grouping variable
#' billboarder() %>% 
#'   bb_densityplot(data = diamonds, x = "depth", group = "cut") %>% 
#'   bb_x_axis(min = 55, max = 70)
#' 
#' # Same with mapping
#' billboarder() %>% 
#'   bb_densityplot(diamonds, bbaes(depth, group = cut)) %>% 
#'   bb_x_axis(min = 55, max = 70)
#' 
#' 
#' # a stacked density plot using count as statistic
#' bb <- billboarder() %>%
#'   bb_densityplot(diamonds, bbaes(depth, group = cut),
#'                  stacked = TRUE, stat = "count") %>%
#'   bb_x_axis(min = 55, max = 70)
#' bb
#' 
#' # changing order
#' bb %>% bb_data(order = "asc")
#' 
bb_densityplot <- function(bb, data, mapping = NULL, stacked = FALSE, stat = "density", fill = FALSE, ...) {
  
  stat <- match.arg(arg = stat, choices = c("density", "count"))
  
  if (missing(data))
    data <- bb$x$data
  
  mapping <- mapping %||% bb$x$mapping
  
  args <- list(...)
  argsdensity <- formals(density.default)
  bw <- args$bw %||% argsdensity$bw
  adjust <- args$adjust %||% argsdensity$adjust
  kernel <- args$kernel %||% "gaussian"
  weights <- args$weights %||% argsdensity$weights
  n <- args$n %||% argsdensity$n
  
  group <- args$group
  
  if (is.vector(data)) {
    if (!is.null(mapping))
      warning("'mapping' is ignored when 'data' is a vector.")
    data <- data.frame(x = data)
    x <- "x"
    if (!is.null(group)) {
      if (!(is.vector(group) | is.factor(group))) {
        stop("If 'data' is a vector, 'group' must be a vector of the same length if specified.")
      }
      data$group <- group
      group <- "group"
    }
  } else {
    if (!is.null(mapping)) {
      if (!is.null(mapping$group))
        group <- as_label(mapping$group)
      else
        group <- NULL
      mapping$group <- NULL
      data_mapped <- bbmapping(data = data, mapping = mapping)
      data_mapped <- data.frame(unlist(data_mapped))
      names(data_mapped) <- as_label(mapping$x)
      if (!is.null(group))
        data_mapped[[group]] <- data[[group]]
      data <- data_mapped
    }
  }
  
  x <- args$x %||% names(data)[1]
  
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
    
    bb <- bb_proxy_axis_labels(proxy = bb, x = x, y = stat)
    
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
#' @param mapping Mapping of variables on the chart, see \code{\link{bbaes}}.
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
#' 
#' data("diamonds", package = "ggplot2")
#' 
#' # one variable
#' billboarder() %>% 
#'   bb_histogram(data = diamonds, x = "price")
#' 
#' # with mapping
#' billboarder() %>% 
#'   bb_histogram(diamonds, bbaes(price))
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
#' # and with mapping
#' billboarder() %>%
#'   bb_histogram(diamonds, bbaes(price, group = cut),
#'                binwidth = 500)
#' 
#' # stacked histogram
#' billboarder() %>%
#'   bb_histogram(diamonds, bbaes(price, group = cut),
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
#' 
bb_histogram <- function(bb, data, mapping = NULL, stacked = FALSE, fill = FALSE, bins = 30, binwidth = NULL, ...) {
  
  if (!requireNamespace(package = "ggplot2"))
    message("Package 'ggplot2' is required to run this function")
  
  if (missing(data))
    data <- bb$x$data
  
  mapping <- mapping %||% bb$x$mapping
  
  args <- list(...)
  group <- args$group
  
  if (is.vector(data)) {
    if (!is.null(mapping))
      warning("'mapping' is ignored when 'data' is a vector.")
    data <- data.frame(x = data)
    x <- "x"
    if (!is.null(group)) {
      if (!(is.vector(group) | is.factor(group))) {
        stop("If 'data' is a vector, 'group' must be a vector of the same length if specified.")
      }
      data$group <- group
      group <- "group"
    }
  } else {
    if (!is.null(mapping)) {
      if (!is.null(mapping$group))
        group <- as_label(mapping$group)
      else
        group <- NULL
      mapping$group <- NULL
      data_mapped <- bbmapping(data = data, mapping = mapping)
      data_mapped <- data.frame(unlist(data_mapped))
      names(data_mapped) <- as_label(mapping$x)
      if (!is.null(group))
        data_mapped[[group]] <- data[[group]]
      data <- data_mapped
    }
  }
  
  x <- args$x %||% names(data)[1]
  
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
    
    bb <- bb_proxy_axis_labels(proxy = bb, x = x, y = "count")
    
    bb <- bb_load(proxy = bb, json = json, x = x, unload = bb$unload) 
    
  } else {
    
    bb <- .bb_opt2(bb, "data", data_opt)
    bb <- .bb_opt(bb, "legend", show = !is.null(group))
    bb <- .bb_opt2(bb, "axis", axis_opt)
    bb <- .bb_opt2(bb, "tooltip", tooltip_opt)
    bb <- .bb_opt(bb, "point", show = FALSE)
    
  }
  
  return(bb)
}








#' Helper for creating a lollipop chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param data A \code{data.frame}, the first column will be used for x axis unless
#' argument \code{x} is specified, the second one will be use as y values.
#'  If not a \code{data.frame}, an object coercible to \code{data.frame}. 
#' @param mapping Mapping of variables on the chart, see \code{\link{bbaes}}.
#' @param rotated Switch x and y axis position.
#' @param point_color Color of the lollipop.
#' @param point_size Size of the lollipop.
#' @param line_color Color of the lines between the axis and the lollipop.
#' @param ... Not used.
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
#' @examples
#' 
#' # From wikipedia
#' sw <- data.frame(
#'   film = c("The Force Awakens", "The Phantom Menace", 
#'            "Revenge of the Sith", "A New Hope",
#'            "Attack of the Clones", "The Empire Strikes Back",
#'            "Return of the Jedi"
#'   ),
#'   worldwide_gross = c(2068178225, 1027044677, 848754768,
#'                       775398007, 649398328, 538375067,
#'                       475106177)
#' )
#' 
#' # Simple example
#' billboarder() %>% 
#'   bb_lollipop(data = sw)
#' 
#' # Fancy example
#' billboarder() %>% 
#'   bb_lollipop(data = sw, rotated = TRUE)%>% 
#'   bb_y_grid(show = TRUE) %>% 
#'   bb_y_axis(tick = list(
#'     values = c(0, 5e+08, 1e+09, 1.5e+09, 2e+09),
#'     outer = FALSE,
#'     format = htmlwidgets::JS("d3.formatPrefix('$,.0', 1e6)")
#'   )) %>% 
#'   bb_x_axis(tick = list(centered = TRUE)) %>% 
#'   bb_labs(
#'     title = "Star Wars - Total Lifetime Grosses",
#'     caption = "Data source : wikipedia"
#'   )
#' 
#' 
#' # With mapping
#' billboarder(data = sw) %>% 
#'   bb_lollipop(mapping = bbaes(x = film, y = worldwide_gross))
#'   
bb_lollipop <- function(bb, data, mapping = NULL, rotated = FALSE, point_color = "#112446", point_size = 8, line_color = "#000", ...) {
  
  if (missing(data))
    data <- bb$x$data
  data <- as.data.frame(data)
  mapping <- mapping %||% bb$x$mapping
  args <- list(...)
  
  
  if (!is.null(mapping)) {
    x <- as_label(mapping$x)
    y <- as_label(mapping$y)
    json <- bbmapping(data = data, mapping = mapping)
  } else {
    x <- names(data)[1]
    y <- names(data)[2]
    json <- bbmapping(data = data, mapping = bbaes_string(x = x, y = y))
  }
  json[["lollipop"]] <- json[[2]]
  
  data_opt <- list(
    x = x,
    json = json,
    type = "bar",
    classes = stats::setNames(list("lollipop-lines"), y),
    types = stats::setNames(c(list("bar", "line")), c("lollipop", y)),
    colors = stats::setNames(c(list(line_color, point_color)), c("lollipop", y))
  )
  
  if ("billboarder_Proxy" %in% class(bb)) {
    
    bb <- bb_load(proxy = bb, json = data_opt$json, unload = bb$unload) 
    
  } else {
    
    bb <- .bb_opt2(bb, "data", data_opt)
    
    bb <- .bb_opt(bb, "axis", x = list(type = "category"), rotated = rotated)
    
    bb <- .bb_opt(bb, "bar", width = 1)
    
    bb <- .bb_opt(bb, "point", r = point_size)
    
    # bb <- .bb_opt(bb, "legend", hide = "lollipop")
    if (rotated) {
      bb <- .bb_opt(bb, "legend", hide = TRUE)
    } else {
      bb <- .bb_opt(bb, "legend", show = FALSE)
    }
    
    bb <- bb_add_style(
      bb = bb, 
      ".bb-target-lollipop-lines > .bb-circle" = "opacity: 1;", 
      ".bb-target-lollipop-lines > .bb-lines" = "opacity: 0;"
    )
    
    bb <- .bb_opt(bb, "tooltip", format = list(
      value = htmlwidgets::JS("function(value, ratio, id, index) {if (id !== 'lollipop') return value; }")
    ))
    
  }
  
  return(bb)
}








#' Helper for creating a radar chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param data A \code{data.frame}, the first column will be used for x axis unless
#' specified otherwise in \code{mapping}. If not a \code{data.frame}, an object coercible to \code{data.frame}.
#' @param mapping Mapping of variables on the chart, see \code{\link{bbaes}}.
#' @param ... Arguments passed to \code{\link{bb_radar}}.
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
#' @examples
#' library("billboarder")
#' 
#' # data about Avengers
#' data("avengers_wide")
#' 
#' # if not specified, first column is used as x-axis, 
#' # all others are used on y-axis
#' billboarder() %>%
#'   bb_radarchart(data = avengers_wide)
#' 
#' # specify explicitly which column to use with mapping
#' billboarder() %>%
#'   bb_radarchart(
#'     data = avengers_wide,
#'     mapping = bbaes(x = axis, y = `Captain America`)
#'   )
#' 
#' 
#' # with data in "long" format you can use "group" aesthetics
#' data("avengers")
#' billboarder() %>%
#'   bb_radarchart(
#'     data = avengers, 
#'     mapping = bbaes(x = axis, y = value, group = group)
#'   )
bb_radarchart <- function(bb, data, mapping = NULL, ...) {
  
  if (missing(data))
    data <- bb$x$data
  
  data <- as.data.frame(data)
  args <- list(...)
  
  mapping <- mapping %||% bb$x$mapping
  
  if (is.null(mapping)) {
    
    x <- names(data)[1]
    y <- names(data)[-1]
    
    if (nrow(data) == 1) {
      json <- lapply(X = as.list(data[c(x, y)]), FUN = list)
    } else {
      json <- as.list(data[c(x, y)])
    }
    names(json)[which(names(json) == x)] <- getOption("billboarder-x", default = "bb-x")
    data_opt <- list(
      x = getOption("billboarder-x", default = "bb-x"),
      json = json,
      type = "radar"
    )
    
  } else {
    
    json <- bbmapping(data = data, mapping = mapping)
    x <- as_label(mapping$x)
    names(json)[which(names(json) == x)] <- getOption("billboarder-x", default = "bb-x")
    
  }
  
  data_opt <- list(
    x = getOption("billboarder-x", default = "bb-x"),
    json = json,
    type = "radar"
  )
  
  if ("billboarder_Proxy" %in% class(bb)) {
    
    bb <- bb_load(
      proxy = bb,
      json = json, 
      x = getOption("billboarder-x", default = "bb-x"),
      unload = bb$unload
    ) 
    
  } else {
    
    bb <- .bb_opt2(bb, "data", dropNulls(data_opt))
    
    bb <- .bb_opt(bb, "radar", ...)
    
  }
  
  return(bb)
}





#' Helper for creating a treemap chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param data A \code{data.frame}, the first column will be used for x axis unless
#'  specified otherwise in \code{mapping}. If not a \code{data.frame}, an object coercible to \code{data.frame}.
#' @param mapping Mapping of variables on the chart, see \code{\link{bbaes}}.
#' @param ... Arguments passed to \code{\link{bb_treemap}}.
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
#' @examples
#' library("billboarder")
#' data("mpg", package = "ggplot2")
#' 
#' billboarder() %>% 
#'   bb_treemapchart(mpg[, 1])
#' 
#' billboarder() %>% 
#'   bb_treemapchart(
#'     data = mpg, 
#'     mapping = aes(x = manufacturer),
#'     label = list(show = TRUE, threshold = 0.3)
#'   ) %>% 
#'   bb_data(
#'     labels = list(colors = "#FFF")
#'   )
bb_treemapchart <- function(bb, data, mapping = NULL, ...) {
  if (missing(data))
    data <- bb$x$data
  
  data <- as.data.frame(data)
  args <- list(...)
  
  mapping <- mapping %||% bb$x$mapping
  
  if (is.null(mapping)) {
    
    if (ncol(data) < 2) {
      data <- as.data.frame(table(x = data[[1]]), responseName = "n")
    }
    
  } else {
    
    if (is.null(mapping$x))
      stop("bb_treemap: 'x' aesthetic must be provided", call. = FALSE)
    
    if (is.null(mapping$y))
      mapping <- aes(!!!mapping, y = rep(1, length(!!mapping$x)))
    
    data <- bbmapping(data = data, mapping = mapping)
    
  }
  
  json <- setNames(lapply(data[[2]], as.list), data[[1]])
  data_opt <- list(
    json = json,
    type = "treemap"
  )
  
  if ("billboarder_Proxy" %in% class(bb)) {
    
    bb <- bb_load(
      proxy = bb,
      json = json, 
      x = getOption("billboarder-x", default = "bb-x"),
      unload = bb$unload
    ) 
    
  } else {
    
    bb <- .bb_opt2(bb, "data", dropNulls(data_opt))
    
    bb <- .bb_opt(bb, "treemap", ...)
    
  }
  
  return(bb)
}


