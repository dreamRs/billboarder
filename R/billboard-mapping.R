#' Map variables on the chart
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param ... Mapping parameters, such as \code{x} for x-axis, \code{y} for y-axis, \code{group} for grouping variable.
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#' 
#' @importFrom ggplot2 aes
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
bb_aes <- function(bb, ...) {
  bb$x$mapping <- bbaes(...)
  bb
}

#' @rdname billboard-aes
#' @export
bb_aes_string <- function(bb, ...) {
  bb$x$mapping <- bbaes_string(...)
  bb
}

#' @rdname billboard-aes
#' @export
#' @importFrom ggplot2 aes
bbaes <- function(...) {
  mapping <- dropNulls(aes(...))
  noname <- which(names(mapping) == "")
  if (length(noname) == 1) {
    names(mapping)[noname] <- "group"
  }
  mapping
}

#' @rdname billboard-aes
#' @export
bbaes_string <- function(...) {
  mapping <- dropNulls(aes_string(...))
  noname <- which(names(mapping) == "")
  if (length(noname) == 1) {
    names(mapping)[noname] <- "group"
  }
  mapping
}


#' @importFrom rlang eval_tidy as_label
bbmapping <- function(data, mapping) {
  
  mapping <- dropNulls(mapping)
  
  # if (is.null(data))
  #   return(list())
  
  if (is.null(mapping$group)) {
    json <- lapply(
      X = mapping,
      FUN = eval_tidy,
      data = data
    )
    if (inherits(json[["x"]], what = c("character", "factor")) & anyDuplicated(json[["x"]])) {
      json[["y"]] <- tapply(X = json[["y"]], INDEX = json[["x"]], FUN = sum, na.rm = TRUE)
      json[["x"]] <- names(json[["y"]])
      json[["y"]] <- as.vector(unname(json[["y"]]))
      message("Non unique values in '", as_label(mapping$x), "' : calculating sum of '", as_label(mapping$y), "'")
    }
    if (!is.null(mapping$ymin) & !is.null(mapping$ymax)) {
      json[["y"]] <- lapply(
        X = seq_along(json[["y"]]),
        FUN = function(i) {
          lapply(X = list(low = json[["ymin"]], mid = json[["y"]], high = json[["ymax"]]), FUN = `[[`, i)
        }
      )
      json[["ymin"]] <- NULL
      json[["ymax"]] <- NULL
    }
    names(json)[names(json) == "x"] <- as_label(mapping$x)
    names(json)[names(json) == "y"] <- as_label(mapping$y)
  } else {
    grouping <- as.character(eval_tidy(mapping$group, data = data))
    grouping_order <- unique(grouping)
    mapping$group <- NULL
    x_un <- eval_tidy(mapping$x, data = data)
    x_un <- unique(x_un)
    data_split <- split(x = data, f = grouping)
    n_ <- names(data_split)
    json <- lapply(
      X = stats::setNames(n_, n_),
      FUN = function(iii) {
        if (!is.null(mapping$y)) {
          if (!is.null(mapping$ymin) & !is.null(mapping$ymax)) {
            ymin_ <- eval_tidy(mapping$ymin, data = data_split[[iii]])
            ymax_ <- eval_tidy(mapping$ymax, data = data_split[[iii]])
            y_ <- eval_tidy(mapping$y, data = data_split[[iii]])
            x_ <- eval_tidy(mapping$x, data = data_split[[iii]])
            idx <- match(x = x_un, table = x_, nomatch = nrow(data_split[[iii]])+1)
            res <- lapply(
              X = seq_along(y_),
              FUN = function(i) {
                lapply(X = list(low = ymin_, mid = y_, high = ymax_), FUN = `[[`, i)
              }
            )
            res[idx]
          } else {
            y_ <- eval_tidy(mapping$y, data = data_split[[iii]])
            x_ <- eval_tidy(mapping$x, data = data_split[[iii]])
            idx <- match(x = x_un, table = x_, nomatch = nrow(data_split[[iii]])+1)
            y_[idx]
          }
        } else {
          eval_tidy(mapping$x, data = data_split[[iii]])
        }
      }
    )
    json <- json[grouping_order]
    if (!is.null(mapping$x)) {
      x <- as_label(mapping$x)
      json[[x]] <- x_un
      json <- json[c(x, setdiff(names(json), x))]
    }
  }
  
  return(json)
}





