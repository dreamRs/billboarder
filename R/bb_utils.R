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
    .bb_proxy(bb, "data", json = as.list(data), ...)
  }

}

#' Add axis parameters
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param ... Arguments defined in \url{https://naver.github.io/billboard.js/demo/}.
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
bb_axis <- function(bb, ...) {

  .bb_opt(bb, "axis", ...)

}


#' Add legend parameters
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param ... Arguments defined in \url{https://naver.github.io/billboard.js/demo/}.
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
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
#' @param ... See \link{https://naver.github.io/billboard.js/release/latest/doc/Options.html#.point}
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





# dropNulls
dropNulls <- function (x)
{
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}


`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}
