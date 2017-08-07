
# dropNulls
dropNulls <- function (x)
{
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
  
  if(!any(class(bb) %in% c("billboarder", "billboarder_Proxy"))){
    stop("bb must be a billboarder or a billboarderProxy object")
  }

  if (is.null(bb$x$bb_opts[[name]])) {
    bb$x$bb_opts[[name]] <- list(...)
  } else {
    bb$x$bb_opts[[name]] <- modifyList(x = bb$x$bb_opts[[name]], val = list(...))
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

  if (is.null(bb$x$bb_opts[[name]])) {
    bb$x$bb_opts[[name]] <- l
  } else {
    bb$x$bb_opts[[name]] <- modifyList(x = bb$x$bb_opts[[name]], val = l)
  }

  return(bb)
}


#' Add data to Billboard chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param data A \code{data.frame}
#' @param ... Arguments defined in \url{https://naver.github.io/billboard.js/demo/}.
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
bb_data <- function(bb, data = NULL, ...) {

  if ("billboarder" %in% class(bb)) {
    .bb_opt(bb, "data", json = as.list(data), ...)
  } else if ("billboarder_Proxy" %in% class(bb)) {
    
    if (nrow(data) == 1) {
      json <- lapply(X = as.list(data), FUN = list)
    } else {
      json <- as.list(data)
    }
    
    .bb_proxy(bb, "data", json = json, ...)
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
#' @param position A string speciefing the position of the title.
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#' 
#' @seealso bb_labs
#' 
#' @examples 
#' billboarder() %>% 
#'   bb_barchart(data = table(sample(letters, 100, TRUE))) %>% 
#'   bb_title(text = "Random letters", position = "center")
#' 
bb_title <- function(bb, text = NULL, padding = NULL, position = "top-center") {

  .bb_opt2(bb, "title", dropNulls(list(text = text, padding = padding, position = position)))

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
#'   bb_scatterplot(data = iris, x = "Sepal.Length", y = "Sepal.Width", group = "Species") %>% 
#'   bb_axis(x = list(tick = list(fit = FALSE))) %>% 
#'   bb_point(r = 8) %>% 
#'   bb_colors_manual(setosa = "#440154", virginica = "#21908C", versicolor = "#FDE725")
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
# @examples
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
# @examples
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
# @examples
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
# @examples
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
# @examples
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





#' Subchart property for a Billboard.js chart
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



#' Regions property for a Billboard.js chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param ... See \url{https://naver.github.io/billboard.js/release/latest/doc/Options.html#.regions}
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
# @examples
bb_regions <- function(bb, ...) {
  
  .bb_opt(bb, "regions", ...)
  
}
