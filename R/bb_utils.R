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
#'   bb_bar(data = stars) %>% 
#'   bb_legend(show = FALSE)
#' 
#' # Right legend
#' billboarder() %>%
#'   bb_pie(data = stars) %>% 
#'   bb_legend(position = "right")
#' 
#' # Inset legend
#' billboarder() %>%
#'   bb_scatter(data = iris, x = "Sepal.Length", y = "Sepal.Width", group = "Species") %>% 
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
#' \dontrun{
#' # Set point size
#' billboarder() %>% 
#'   bb_scatter(data = iris, x = "Sepal.Length", y = "Sepal.Width", group = "Species") %>% 
#'   bb_axis(x = list(tick = list(fit = FALSE))) %>% 
#'   bb_point(r = 10)
#' }
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
#' \dontrun{
#' # Format tooltip
#' billboarder() %>% 
#'   bb_scatter(data = iris, x = "Sepal.Length", y = "Sepal.Width", group = "Species") %>% 
#'   bb_tooltip(
#'     format = list(
#'       # skip the title in tooltip
#'       title = htmlwidgets::JS("function() {return undefined;}"),
#'       name = htmlwidgets::JS("function(name, ratio, id, index) {return '';}"),
#'       value = htmlwidgets::JS("function(value, ratio, id, index) {return id;}")
#'     )
#'   )
#' }
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
#' \dontrun{
#' 
#' # Scatter
#' billboarder() %>% 
#'   bb_scatter(data = iris, x = "Sepal.Length", y = "Sepal.Width", group = "Species") %>% 
#'   bb_axis(x = list(tick = list(fit = FALSE))) %>% 
#'   bb_point(r = 8) %>% 
#'   bb_color(palette = RColorBrewer::brewer.pal(n = 3, name = "Reds"))
#'
#' # Pie
#' stars <- data.frame(
#'   package = c("billboarder", "ggiraph", "officer", "shinyWidgets", "visNetwork"),
#'   stars = c(9, 177, 43, 44, 169)
#' )
#' cols <- RColorBrewer::brewer.pal(n = 5, name = "Dark2")
#' 
#' billboarder() %>%
#'   bb_pie(data = stars) %>%
#'   bb_color(palette = RColorBrewer::brewer.pal(n = 5, name = "Reds"))
#' 
#' }
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
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
#' @examples
#' \dontrun{
#'  
#' # Scatter
#' billboarder() %>% 
#' bb_scatter(data = iris, x = "Sepal.Length", y = "Sepal.Width", group = "Species") %>% 
#'   bb_axis(x = list(tick = list(fit = FALSE))) %>% 
#'   bb_point(r = 8) %>% 
#'   bb_colors_manual(setosa = "#440154", virginica = "#21908C", versicolor = "#FDE725")
#' 
#' # Pie
#' stars <- data.frame(
#'   package = c("billboarder", "ggiraph", "officer", "shinyWidgets", "visNetwork"),
#'   stars = c(9, 177, 43, 44, 169)
#' )
#' cols <- RColorBrewer::brewer.pal(n = 5, name = "Dark2")
#' 
#' billboarder() %>% 
#'   bb_pie(data = stars) %>% 
#'   bb_colors_manual(
#'    setNames(as.list(cols), stars$package) # this is a named list
#'   )
#'   
#' }
bb_colors_manual <- function(bb, ...) {
  
  args <- list(...)
  args <- as.list(unlist(args))
  .bb_opt(bb, "data", colors = args)
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
#' \dontrun{
#' 
#' stars <- data.frame(
#'   package = c("billboarder", "ggiraph", "officer", "shinyWidgets", "visNetwork"),
#'   stars = c(1, 176, 42, 40, 166)
#' )
#' 
#' billboarder() %>%
#'   bb_bar(data = stars) %>% 
#'   bb_y_grid(show = TRUE)
#' 
#' billboarder() %>%
#'   bb_bar(data = stars) %>% 
#'   bb_y_grid(lines = list(list(value = mean(stars$stars), text = "Horizontal line")))
#' 
#' }
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



# dropNulls
dropNulls <- function (x)
{
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}


`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}
