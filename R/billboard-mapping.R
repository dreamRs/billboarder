#' Map variables on the chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param x Name of the variable to map on the x-axis.
#' @param y Name of the variable to map on the y-axis.
#' @param group Name of the grouping variable.
#' @param ... Additional mapping parameters, for now only 'size' for scatter plot is used.
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#' 
#' @note \code{bb_aes} is intended to use in a "piping" way. 
#' \code{bbaes} is the equivalent to use inside a helper function
#'  such as \code{bb_barchart}, \code{bb_scatterplot}...
#' 
#' @name billboard-aes
#'
#' @examples
#' \dontrun{
#' dat <- as.data.frame(table(sample(letters[1:5], 100, TRUE)))
#' 
#' billboarder(data = dat) %>% 
#'   bb_aes(x = Var1, y = Freq) %>% 
#'   bb_barchart()
#' 
#' 
#' tab <- table(sample(letters[1:5], 100, TRUE), sample(LETTERS[1:5], 100, TRUE))
#' dat_group <- as.data.frame(tab)
#' 
#' billboarder(data = dat_group) %>% 
#'   bb_aes(x = Var1, y = Freq, group = "Var2") %>% 
#'   bb_barchart()
#' }
# bbaes <- function(bb, x, y, group = NULL) {
#   x <- deparse(substitute(x))
#   y <- deparse(substitute(y))
#   group <- deparse(substitute(group))
#   if (identical(group, "NULL"))
#     group <- NULL
#   bb$x$aes <- list(x = x, y = y, group = group)
#   bb
# }
bb_aes <- function(bb, x, y, group = NULL, ...) {
  mapping <- structure(as.list(match.call()[-1]), class = "uneval")
  mapping$bb <- NULL
  bb$x$mapping <- mapping
  bb
}

#' @rdname billboard-aes
#' @export
bb_aes_string <- function(bb, x, y, group = NULL, ...) {
  mapping <- list()
  if (!missing(x)) 
    mapping["x"] <- list(x)
  if (!missing(y)) 
    mapping["y"] <- list(y)
  if (!is.null(group)) 
    mapping["group"] <- list(group)
  args <- list(...)
  if (!is.null(args$size)) 
    mapping["size"] <- list(args$size)
  mapping <- lapply(
    X = mapping,
    FUN = function(x) {
      if (is.character(x)) {
        parse(text = x)[[1]]
      }
      else {
        x
      }
    }
  )
  bb$x$mapping <- structure(mapping, class = "uneval")
  bb
}

#' @rdname billboard-aes
#' @export
bbaes <- function(x, y, group = NULL, ...) {
  mapping <- structure(as.list(match.call()[-1]), class = "uneval")
  mapping
}

#' @rdname billboard-aes
#' @export
bbaes_string <- function(x, y, group = NULL, ...) {
  mapping <- list()
  if (!missing(x)) 
    mapping["x"] <- list(x)
  if (!missing(y)) 
    mapping["y"] <- list(y)
  if (!is.null(group)) 
    mapping["group"] <- list(group)
  args <- list(...)
  if (!is.null(args$size)) 
    mapping["size"] <- list(args$size)
  mapping <- lapply(
    X = mapping,
    FUN = function(x) {
      if (is.character(x)) {
        parse(text = x)[[1]]
      }
      else {
        x
      }
    }
  )
  aes <- structure(mapping, class = "uneval")
  aes
}


bbmapping <- function(data, mapping) {
  
  # if (is.null(data))
  #   return(list())
  
  if (is.null(mapping$group)) {
    json <- lapply(
      X = mapping,
      FUN = function(paraes) {
        eval(paraes, envir = data, enclos = parent.frame())
      }
    )
    names(json) <- as.character(unlist(mapping))
    x <- as.character(mapping$x)
    if (inherits(json[[x]], what = c("character", "factor")) & anyDuplicated(json[[x]])) {
      y <- as.character(mapping$y)
      json[[y]] <- tapply(X = json[[y]], INDEX = json[[x]], FUN = sum, na.rm = TRUE)
      json[[x]] <- names(json[[y]])
      json[[y]] <- as.vector(unname(json[[y]]))
      message("Non unique values in '", x, "' : calculating sum of '", y, "'")
    }
    if (!is.null(mapping$ymin) & !is.null(mapping$ymax)) {
      json[[mapping$y]] <- lapply(
        X = seq_along(json[[mapping$y]]),
        FUN = function(i) {
          lapply(X = list(low = json[[mapping$ymin]], mid = json[[mapping$y]], high = json[[mapping$ymax]]), FUN = `[[`, i)
        }
      )
      json[[mapping$ymin]] <- NULL
      json[[mapping$ymax]] <- NULL
    }
  } else {
    grouping <- eval(mapping$group, envir = data, enclos = parent.frame())
    mapping$group <- NULL
    x_un <- eval(mapping$x, envir = data, enclos = parent.frame())
    x_un <- unique(x_un)
    data_split <- split(x = data, f = grouping)
    n_ <- names(data_split)
    json <- lapply(
      X = stats::setNames(n_, n_),
      FUN = function(iii) {
        if (!is.null(mapping$y)) {
          if (!is.null(mapping$ymin) & !is.null(mapping$ymax)) {
            ymin_ <- eval(mapping$ymin, envir = data_split[[iii]], enclos = parent.frame())
            ymax_ <- eval(mapping$ymax, envir = data_split[[iii]], enclos = parent.frame())
            y_ <- eval(mapping$y, envir = data_split[[iii]], enclos = parent.frame())
            x_ <- eval(mapping$x, envir = data_split[[iii]], enclos = parent.frame())
            idx <- match(x = x_un, table = x_, nomatch = nrow(data_split[[iii]])+1)
            res <- lapply(
              X = seq_along(y_),
              FUN = function(i) {
                lapply(X = list(low = ymin_, mid = y_, high = ymax_), FUN = `[[`, i)
              }
            )
            res[idx]
          } else {
            y_ <- eval(mapping$y, envir = data_split[[iii]], enclos = parent.frame())
            x_ <- eval(mapping$x, envir = data_split[[iii]], enclos = parent.frame())
            idx <- match(x = x_un, table = x_, nomatch = nrow(data_split[[iii]])+1)
            y_[idx]
          }
        } else {
          eval(mapping$x, envir = data_split[[iii]], enclos = parent.frame())
        }
      }
    )
    if (!is.null(mapping$y)) {
      x <- as.character(mapping$x)
      json[[x]] <- x_un
      json <- json[c(x, setdiff(names(json), x))]
    }
  }
  
  return(json)
  
}





