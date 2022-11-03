
# dropNulls
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}


`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}



#' Utility function to create Billboard parameters JSON
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param name Slot's name to edit
#' @param ... Arguments for the slot
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' 
#' @importFrom utils modifyList
#'
#' @noRd
.bb_opt <- function(bb, name, ...) {

  bb$x$bb_empty <- NULL
  if (is.null(bb$x$bb_opts[[name]])) {
    bb$x$bb_opts[[name]] <- list(...)
  } else {
    bb$x$bb_opts[[name]] <- utils::modifyList(
      x = bb$x$bb_opts[[name]], 
      val = list(...), 
      keep.null = TRUE
    )
  }

  return(bb)
}

#' Utility function to create Billboard parameters JSON
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param name Slot's name to edit
#' @param l List of arguments for the slot
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#'
#' @noRd
.bb_opt2 <- function(bb, name, l) {

  bb$x$bb_empty <- NULL
  if (is.null(bb$x$bb_opts[[name]])) {
    bb$x$bb_opts[[name]] <- l
  } else {
    bb$x$bb_opts[[name]] <- utils::modifyList(
      x = bb$x$bb_opts[[name]], 
      val = l,
      keep.null = TRUE
    )
  }

  return(bb)
}


#' Add data to Billboard chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param ... Arguments defined in \url{https://naver.github.io/billboard.js/demo/}.
#' 
#' @note This function can be used with \code{\link{billboarderProxy}} in shiny application.
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
#' @examples 
#' billboarder() %>%
#'  bb_barchart(data = table(mtcars$cyl)) %>%
#'  bb_data(names = list(Freq = "Number of cylinders"), labels = TRUE)
#'  
bb_data <- function(bb, ...) {
  
  args <- list(...)

  if ("billboarder" %in% class(bb)) {
    
    if (is.null(bb$x$mapping)) {
      
      .bb_opt(bb, "data", ...)
      
    } else {
      
      data <- args$data %||% bb$x$data

      .bb_opt(bb, "data", json = bbmapping(data = data, mapping = bb$x$mapping), x = "x", ...)
      
    }
    
  } else if ("billboarder_Proxy" %in% class(bb)) {
    
    if (!is.null(args$data)) {
      data <- args$data
      if (nrow(data) == 1) {
        json <- lapply(X = as.list(data), FUN = list)
      } else {
        json <- as.list(data)
      }
      
      .bb_proxy(bb, "data", json = json, ...)
    } else {
      .bb_proxy(bb, "data", ...)
    }
    
  }

}

#' Add axis parameters
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param ... Arguments defined in \url{https://naver.github.io/billboard.js/demo/}.
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#' 
#' @name bb_axis
#' 
#' @examples 
#' 
#' stars <- data.frame(
#'   package = c("billboarder", "ggiraph", "officer", "shinyWidgets", "visNetwork"),
#'   stars = c(9, 178, 43, 46, 175)
#' )
#'
#' # Add a label to y axis
#' billboarder() %>% 
#'   bb_barchart(data = stars) %>% 
#'   bb_axis(y = list(label = list(text = "# of stars", position = "middle")))
#'   
#' # or shorter :
#' billboarder() %>% 
#'   bb_barchart(data = stars) %>% 
#'   bb_y_axis(label = list(text = "# of stars", position = "outer-top"))
#' 
bb_axis <- function(bb, ...) {

  .bb_opt(bb, "axis", ...)

}

#' @rdname bb_axis
#' @export
bb_x_axis <- function(bb, ...) {
  
  .bb_opt(bb, "axis", x = list(...))
  
}

#' @rdname bb_axis
#' @export
bb_y_axis <- function(bb, ...) {
  
  .bb_opt(bb, "axis", y = list(...))
  
}


#' Add legend parameters
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param ... Arguments defined in \url{https://naver.github.io/billboard.js/release/latest/doc/Options.html#.legend}.
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
#' # Hide legend
#' billboarder() %>%
#'   bb_barchart(data = stars) %>% 
#'   bb_legend(show = FALSE)
#' 
#' # Right legend
#' billboarder() %>%
#'   bb_piechart(data = stars) %>% 
#'   bb_legend(position = "right")
#' 
#' # Inset legend
#' billboarder() %>%
#'   bb_scatterplot(data = iris, x = "Sepal.Length", y = "Sepal.Width", group = "Species") %>% 
#'   bb_axis(x = list(tick = list(fit = FALSE))) %>% 
#'   bb_legend(position = "inset", inset = list(anchor = "top-right"))
#' 
bb_legend <- function(bb, ...) {

  .bb_opt(bb, "legend", ...)

}



#' Add title to Billboard.js chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param text The chart title.
#' @param padding A named list with \code{top}, \code{right}, \code{bottom}, \code{left} values.
#' @param position A string specifying the position of the title.
#' @param ... Additional arguments.
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#' 
#' @seealso \link{bb_labs}
#' 
#' @examples 
#' billboarder() %>% 
#'   bb_barchart(data = table(sample(letters, 100, TRUE))) %>% 
#'   bb_title(text = "Random letters", position = "center")
#' 
bb_title <- function(bb, text = NULL, padding = NULL, position = "top-center", ...) {

  .bb_opt2(bb, "title", dropNulls(c(
    list(text = text, padding = padding, position = position),
    list(...)
  )))

}



#' Point property for a Billboard.js chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param ... See \url{https://naver.github.io/billboard.js/release/latest/doc/Options.html#.point}
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
#' @examples
#' # Set point size
#' billboarder() %>% 
#'   bb_scatterplot(data = iris, x = "Sepal.Length", y = "Sepal.Width", group = "Species") %>% 
#'   bb_axis(x = list(tick = list(fit = FALSE))) %>% 
#'   bb_point(r = 10)
bb_point <- function(bb, ...) {
  
  .bb_opt(bb, "point", ...)
  
}



#' Tooltip property for a Billboard.js chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param ... See \url{https://naver.github.io/billboard.js/release/latest/doc/Options.html#.tooltip}
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
#' @examples
#' # Format tooltip
#' billboarder() %>% 
#'   bb_scatterplot(data = iris, x = "Sepal.Length", y = "Sepal.Width", group = "Species") %>% 
#'   bb_tooltip(
#'     format = list(
#'       # skip the title in tooltip
#'       title = htmlwidgets::JS("function() {return undefined;}"),
#'       name = htmlwidgets::JS("function(name, ratio, id, index) {return '';}"),
#'       value = htmlwidgets::JS("function(value, ratio, id, index) {return id;}")
#'     )
#'   )
bb_tooltip <- function(bb, ...) {
  
  .bb_opt(bb, "tooltip", ...)
  
}


#' Color property for a Billboard.js chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param palette A color palette to use with series added in the chart.
#' @param ... See \url{https://naver.github.io/billboard.js/release/latest/doc/Options.html#.color}
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
#' @examples
#' 
#' library("RColorBrewer")
#' 
#' # Scatter
#' billboarder() %>% 
#'   bb_scatterplot(data = iris, x = "Sepal.Length", y = "Sepal.Width", group = "Species") %>% 
#'   bb_axis(x = list(tick = list(fit = FALSE))) %>% 
#'   bb_point(r = 8) %>% 
#'   bb_color(palette = brewer.pal(n = 3, name = "Reds"))
#'
#' # Pie
#' stars <- data.frame(
#'   package = c("billboarder", "ggiraph", "officer", "shinyWidgets", "visNetwork"),
#'   stars = c(9, 177, 43, 44, 169)
#' )
#' cols <- brewer.pal(n = 5, name = "Dark2")
#' 
#' billboarder() %>%
#'   bb_piechart(data = stars) %>%
#'   bb_color(palette = brewer.pal(n = 5, name = "Reds"))
#' 
bb_color <- function(bb, palette = NULL, ...) {
  
  if (length(palette) == 1) {
    palette <- list(palette)
  }
  
  .bb_opt2(bb, "color", c(dropNulls(list(pattern = palette)), ...))
  
}


#' Set colors for each datas
#' 
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param ... A named list, where names correspond to the data, and values
#' to color associate with it.
#' @param opacity Color opacity (for area charts).
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
#' @examples
#'
#' library("RColorBrewer")
#' 
#' # Scatter
#' billboarder() %>% 
#'   bb_scatterplot(
#'    data = iris, 
#'    x = "Sepal.Length", 
#'    y = "Sepal.Width", 
#'    group = "Species"
#'   ) %>% 
#'   bb_axis(x = list(tick = list(fit = FALSE))) %>% 
#'   bb_point(r = 8) %>% 
#'   bb_colors_manual(
#'    setosa = "#440154", 
#'    virginica = "#21908C", 
#'    versicolor = "#FDE725"
#'   )
#' 
#' # Pie
#' stars <- data.frame(
#'   package = c("billboarder", "ggiraph", "officer",
#'               "shinyWidgets", "visNetwork"),
#'   stars = c(9, 177, 43, 44, 169)
#' )
#' cols <- brewer.pal(n = 5, name = "Dark2")
#' 
#' billboarder() %>% 
#'   bb_piechart(data = stars) %>% 
#'   bb_colors_manual(
#'    setNames(as.list(cols), stars$package) # this is a named list
#'   )
#'   
bb_colors_manual <- function(bb, ..., opacity = 1) {
  
  args <- list(...)
  args <- as.list(unlist(args))
  bb <- .bb_opt(bb, "data", colors = args)
  .bb_opt(bb, "billboarderspecials", opacity = opacity)
}




#' Grid property for a Billboard.js chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param ... See \url{https://naver.github.io/billboard.js/release/latest/doc/Options.html#.grid}
#' 
#' @note \code{bb_x_grid} and \code{bb_y_grid} are shortcut for modifying the x-axis and the y-axis respectively.
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#' 
#' @name bb_grid
#'
#' @examples
#' 
#' stars <- data.frame(
#'   package = c("billboarder", "ggiraph", "officer", "shinyWidgets", "visNetwork"),
#'   stars = c(1, 176, 42, 40, 166)
#' )
#' 
#' billboarder() %>%
#'   bb_barchart(data = stars) %>% 
#'   bb_y_grid(show = TRUE)
#' 
#' billboarder() %>%
#'   bb_barchart(data = stars) %>% 
#'   bb_y_grid(lines = list(list(value = mean(stars$stars), text = "Horizontal line")))
#' 
bb_grid <- function(bb, ...) {
  
  .bb_opt(bb, "grid", ...)
  
}

#' @rdname bb_grid
#' @export
bb_x_grid <- function(bb, ...) {
  
  .bb_opt(bb, "grid", x = list(...))
  
}

#' @rdname bb_grid
#' @export
bb_y_grid <- function(bb, ...) {
  
  .bb_opt(bb, "grid", y = list(...))
  
}






#' Interaction property for a Billboard.js chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param ... See \url{https://naver.github.io/billboard.js/release/latest/doc/Options.html#.interaction}
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
# @examples
bb_interaction <- function(bb, ...) {
  
  .bb_opt(bb, "interaction", ...)
  
}





#' Transition property for a Billboard.js chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param ... See \url{https://naver.github.io/billboard.js/release/latest/doc/Options.html#.transition}
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
# @examples
bb_transition <- function(bb, ...) {
  
  .bb_opt(bb, "transition", ...)
  
}




#' Spline property for a Billboard.js chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param ... See \url{https://naver.github.io/billboard.js/release/latest/doc/Options.html#.spline}
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
# @examples
bb_spline <- function(bb, ...) {
  
  .bb_opt(bb, "spline", ...)
  
}


#' Line property for a Billboard.js chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param ... See \url{https://naver.github.io/billboard.js/release/latest/doc/Options.html#.line}
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
#' @examples
#' # Set if null data point will be connected or not.
#' b <- billboarder() %>% 
#'   bb_linechart(data = c(1, 2, NA, 4, 5))
#' b
#' b %>%  bb_line(connectNull = TRUE)
#' 
bb_line <- function(bb, ...) {
  
  .bb_opt(bb, "line", ...)
  
}


#' Pie property for a Billboard.js chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param ... See \url{https://naver.github.io/billboard.js/release/latest/doc/Options.html#.pie}
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
#' @examples
#' billboarder() %>%
#'   bb_piechart(data = table(mtcars$cyl)) %>% 
#'   bb_pie(label = list(
#'     ratio = 0.5, 
#'     format = htmlwidgets::JS("function(value) {return d3.format('$')(value);}")
#'   ), 
#'   expand = FALSE)
#'   
bb_pie <- function(bb, ...) {
  
  .bb_opt(bb, "pie", ...)
  
}

#' Donut property for a Billboard.js chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param ... See \url{https://naver.github.io/billboard.js/release/latest/doc/Options.html#.donut}
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
#' @examples
#' billboarder() %>%
#'   bb_donutchart(data = table(mtcars$cyl)) %>%
#'   bb_donut(title = "Donut Title", width = 10)
#'   
bb_donut <- function(bb, ...) {
  
  .bb_opt(bb, "donut", ...)
  
}


#' Gauge property for a Billboard.js chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param ... See \url{https://naver.github.io/billboard.js/release/latest/doc/Options.html#.gauge}
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
#' @examples
#' billboarder() %>% 
#'   bb_gaugechart(value = 50) %>% 
#'   bb_gauge(min = 0, max = 200, units = "km/h", width = 10,
#'            label = list(format = htmlwidgets::JS("function(value) {return value;}")))
#'            
bb_gauge <- function(bb, ...) {
  
  .bb_opt(bb, "gauge", ...)
  
}



#' Bar property for a Billboard.js chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param ... See \url{https://naver.github.io/billboard.js/release/latest/doc/Options.html#.bar}
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
#' @examples
#' billboarder() %>%
#'   bb_barchart(data = data.frame(v1 = c("a", "b", "c"), value = c(5, 6, 3))) %>% 
#'   bb_bar(width = list(ratio = 0.95))
#'   
bb_bar <- function(bb, ...) {
  
  .bb_opt(bb, "bar", ...)
  
}



#' Area property for a Billboard.js chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param ... See \url{https://naver.github.io/billboard.js/release/latest/doc/Options.html#.area}
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
# @examples
bb_area <- function(bb, ...) {
  
  .bb_opt(bb, "area", ...)
  
}





#' @title Subchart property for a Billboard.js chart
#' 
#' @description Create a subchart allowing to zoom and navigate on the chart.
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param ... See \url{https://naver.github.io/billboard.js/release/latest/doc/Options.html#.subchart}
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
#' @examples
#' 
#' data("equilibre_mensuel")
#' 
#' billboarder() %>% 
#'   bb_linechart(data = equilibre_mensuel[, c("date", "production")], type = "spline") %>% 
#'   bb_subchart(show = TRUE)
#'
bb_subchart <- function(bb, ...) {
  
  .bb_opt(bb, "subchart", ...)
  
}



#' @title Regions property for a Billboard.js chart
#' 
#' @description Add a shading effect to the background of the chart, to highlight a period for example.
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param ... See \url{https://naver.github.io/billboard.js/release/latest/doc/Options.html#.regions}
#'
#' @seealso \code{\link{bb_add_style}}
#' 
#' @note This function can be used with \code{\link{billboarderProxy}} in shiny application.
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
#' @examples
#' #' With a categorical X-axis
#' dat <- data.frame(
#'   month = month.abb,
#'   AirPassengers = tail(AirPassengers, 12)
#' )
#' # Highlight Jun/Jul/Aug
#' billboarder() %>% 
#'   bb_linechart(data = dat, x = "month") %>% 
#'   bb_x_axis(type = "category") %>% 
#'   bb_regions(
#'     list(start = 4.5, end = 7.5) #' jan = 0
#'   )
#' 
#' # With a barchart
#' billboarder() %>% 
#'   bb_barchart(data = dat) %>% 
#'   bb_regions(
#'     list(start = 1.5, end = 2.5, class = "custom"),
#'     list(start = 8, end = 10, class = "foo")
#'   ) %>% 
#'   bb_add_style(region = list(custom = "fill: red;", foo = "fill: #'009246;"))
#' 
#' 
#' 
#' 
#' # With Date X-axis
#' library("stats")
#' dat <- data.frame(
#'   date = seq.Date(from = Sys.Date(), by = "day", length.out = 365),
#'   var = density(rexp(n = 1000), n = 365)$y
#' )
#' 
#' billboarder() %>% 
#'   bb_linechart(data = dat) %>% 
#'   bb_x_axis(tick = list(fit = FALSE)) %>% 
#'   bb_y_axis(min = 0, padding = 0) %>% 
#'   bb_regions(
#'     list(start = format(Sys.Date() + 30), end = format(Sys.Date() + 120))
#'   )
#' 
#' 
#' 
#' # With POSIXct X-axis
#' dat <- data.frame(
#'   time = seq.POSIXt(from = Sys.time(), by = "min", length.out = 60),
#'   var = round(sort(rnorm(60)), 2)
#' )
#' 
#' billboarder() %>% 
#'   bb_linechart(data = dat) %>% 
#'   bb_x_axis(tick = list(format = "%H:%M", fit = FALSE)) %>%  
#'   bb_regions(
#'     list(start = format(dat$time[15]), 
#'          end = format(dat$time[30]))
#'   )
#'
bb_regions <- function(bb, ...) {
  
  if ("billboarder_Proxy" %in% class(bb)) {
    .bb_proxy(bb, "regions",  ...)
  } else {
    .bb_opt(bb, "regions", ...)
  }
  
}




#' Zoom property for a Billboard.js chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param ... See \url{https://naver.github.io/billboard.js/release/latest/doc/Options.html#.zoom}
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
#' @examples
#' # data
#' data("equilibre_mensuel")
#' 
#' # line chart
#' billboarder() %>% 
#'   bb_linechart(
#'     data = equilibre_mensuel[, c("date", "consommation", "production")], 
#'     type = "spline"
#'   ) %>% 
#'   bb_x_axis(tick = list(format = "%Y-%m", fit = FALSE)) %>% 
#'   bb_zoom(enabled = TRUE)
#' 
bb_zoom <- function(bb, ...) {
  
  .bb_opt(bb, "zoom", ...)
  
}




#' Bubble property for a Billboard.js chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param ... See \url{https://naver.github.io/billboard.js/release/latest/doc/Options.html#.bubble}
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
#' @examples
#' billboarder() %>% 
#'   bb_scatterplot(
#'     data = iris, 
#'     mapping = bbaes(Sepal.Length, Sepal.Width, group = Species, size = Petal.Width)
#'   ) %>% 
#'   bb_bubble(maxR = 10)
#' 
#' 
#' billboarder() %>% 
#'   bb_scatterplot(
#'     data = iris, 
#'     mapping = bbaes(Sepal.Length, Sepal.Width, group = Species, size = Petal.Width)
#'   ) %>% 
#'   bb_bubble(maxR = JS("function(d) {return Math.sqrt(d.value.z * 20);}"))
bb_bubble <- function(bb, ...) {
  
  .bb_opt(bb, "bubble", ...)
  
}



#' SVG property for a Billboard.js chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param ... See \url{https://naver.github.io/billboard.js/release/latest/doc/Options.html#.svg}
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
bb_svg <- function(bb, ...) {
  
  .bb_opt(bb, "svg", ...)
  
}




#' Radar property for a Billboard.js chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param ... See \url{https://naver.github.io/billboard.js/release/latest/doc/Options.html#.radar}
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
#' @examples
#' library("billboarder")
#' data("avengers")
#' 
#' # number of levels
#' billboarder() %>%
#'   bb_radarchart(
#'     data = avengers,
#'     mapping = bbaes(x = axis, y = value, group = group)
#'   ) %>% 
#'   bb_radar(level = list(depth = 4))
#' 
#' # hide levels
#' billboarder() %>%
#'   bb_radarchart(
#'     data = avengers,
#'     mapping = bbaes(x = axis, y = value, group = group)
#'   ) %>% 
#'   bb_radar(level = list(show = FALSE))
#' 
#' # max value on axis
#' billboarder() %>%
#'   bb_radarchart(
#'     data = avengers,
#'     mapping = bbaes(x = axis, y = value, group = group)
#'   ) %>% 
#'   bb_radar(axis = list(max = 10))
bb_radar <- function(bb, ...) {
  
  .bb_opt(bb, "radar", ...)
  
}








#' Export a Billboard to PNG
#'
#' @param bb A \code{\link{billboarder}} \code{htmlwidget} object
#'  or a \code{\link{billboarderProxy}} \code{htmlwidget} object.
#' @param filename A string of the filename, excluding extension (will be \code{".png"}).
#' @param download_label Label to appear on the link to download PNG.
#' @param ... Additional arguments (not used).
#' 
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#' 
#' @note This function has two uses:
#' \itemize{
#'  \item{\strong{in shiny:} you can export to PNG with an \code{observeEvent} by using \code{\link{billboarderProxy}}.}
#'  \item{\strong{in markdown and in shiny:} add a button to download chart as PNG.}
#' }
#'
#' @examples
#' 
#' # Add a button to download as PNG:
#' 
#' data("equilibre_mensuel")
#' billboarder() %>% 
#'   bb_linechart(
#'     data = equilibre_mensuel,
#'     mapping = bbaes(date, solde),
#'     type = "spline"
#'   ) %>% 
#'   bb_x_axis(
#'     tick = list(format = "%Y-%m", fit = FALSE)
#'   ) %>% 
#'   bb_export(
#'     filename = "my-awesome-chart",
#'     download_label = "Click to download"
#'   )
#'   
#'
#' # In shiny, you can use proxy :
#' 
#' if (interactive()) {
#'   library(shiny)
#'   library(billboarder)
#'   
#'   ui <- fluidPage(
#'     fluidRow(
#'       column(
#'         width = 8, offset = 2,
#'         tags$h1("Export billboard as PNG via Proxy"),
#'         billboarderOutput(outputId = "mybb"),
#'         actionButton(
#'           inputId = "export", 
#'           label = "Export", 
#'           icon = icon("download")
#'         )
#'       )
#'     )
#'   )
#'   
#'   server <- function(input, output, session) {
#'     
#'     output$mybb <- renderBillboarder({
#'       data("prod_par_filiere")
#'       billboarder() %>%
#'         bb_barchart(
#'           data = prod_par_filiere[, c("annee", "prod_hydraulique")],
#'           color = "#102246"
#'         ) %>%
#'         bb_y_grid(show = TRUE)
#'     })
#'     
#'     observeEvent(input$export, {
#'       billboarderProxy(shinyId = "mybb") %>% 
#'         bb_export(filename = "my-billboard-chart")
#'     })
#'     
#'   }
#'   
#'   shinyApp(ui, server)
#' }
bb_export <- function(bb, filename = NULL, download_label = "Export (.png)", ...) {
  if (is.null(filename))
    filename <- paste0("export-", Sys.time())
  if (inherits(bb, "billboarder_Proxy")) {
    .bb_proxy(bb, "export", filename = filename)
  } else {
    .bb_opt(bb, "export", filename = filename, download_label = download_label)
  }
}




#' Render property for a Billboard.js chart
#'
#' @param bb A \code{\link{billboarder}} \code{htmlwidget} object
#'  or a \code{\link{billboarderProxy}} \code{htmlwidget} object.
#' @param ... See \url{https://naver.github.io/billboard.js/release/latest/doc/Options.html#.render} for possible options.
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
bb_render <- function(bb, ...) {
  
  .bb_opt(bb, "render", ...)
  
}



