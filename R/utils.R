
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}

list1 <- function(x) {
  if (length(x) == 1) {
    list(x)
  } else {
    x
  }
}

#' Shortcut to add a suffix value to axis labels
#'
#' @param x A character of length one.
#'
#' @export
#' @seealso prefix
#' @importFrom htmlwidgets JS
#'
# @examples
suffix <- function(x) {
  if (length(x) != 1)
    stop("'x' must be of length one")
  htmlwidgets::JS(
    sprintf("function(x) {return x + '%s';}", x)
  )
}

#' Shortcut to add a prefix value to axis labels
#'
#' @param x A character of length one.
#'
#' @export
#' @seealso suffix
#' @importFrom htmlwidgets JS
#'
# @examples
prefix <- function(x) {
  if (length(x) != 1)
    stop("'x' must be of length one")
  htmlwidgets::JS(
    sprintf("function(x) {return '%s' + x;}", x)
  )
}