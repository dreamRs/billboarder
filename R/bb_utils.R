#' Utility function to create Billboard parameters JSON
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param name Slot's name to edit
#' @param ... Arguments for the slot
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#'
#' @noRd
.bb_opt <- function(bb, name, ...) {

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
#' @param ... Arguments defined in \url{https://naver.github.io/billboard.js/demo/}.
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
bb_data <- function(bb, ...) {

  .bb_opt(bb, "data", ...)

}

#' Add axis parameters
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param ... Arguments defined in \url{https://naver.github.io/billboard.js/demo/}.
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
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
#' @examples
#' \dontrun{
#'
#' }
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
#' @examples
#' \dontrun{
#'
#' }
bb_title <- function(bb, text = NULL, padding = NULL, position = "top-center") {

  .bb_opt2(bb, "title", dropNulls(list(text = text, padding = padding, position = position)))

}




#' Create a bar chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param data A \code{data.frame}, the first column will be used for x axis unless
#' specified otherwise in \code{...}
#' @param ... Arguments for slot data and bar
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
bb_bar <- function(bb, data, ...) {

  args <- list(...)

  data_names <- setdiff(names(args), c("width", "zerobased"))
  data_opt <- list(
    x = args$x %||% names(data)[1],
    json = as.list(data),
    type = "bar"
  )
  data_opt <- c(data_opt, args[names(args) %in% data_names])
  bb <- .bb_opt2(bb, "data", data_opt)

  bb <- .bb_opt2(bb, "bar", args[names(args) %in% c("width", "zerobased")])

  return(bb)
}





# dropNulls
dropNulls <- function (x)
{
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}


`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}
