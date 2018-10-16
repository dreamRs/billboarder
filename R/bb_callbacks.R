

#' Callbacks for billboard charts
#'
#' @param bb A \code{billboard} \code{htmlwidget} object.
#' @param onafterinit Set a callback to execute after the chart is initialized.
#' @param onbeforeinit Set a callback to execute before the chart is initialized.
#' @param oninit Set a callback to execute when the chart is initialized.
#' @param onout Set a callback to execute when mouse/touch leaves the chart.
#' @param onover Set a callback to execute when mouse/touch enters the chart.
#' @param onrendered Set a callback which is executed when the chart is rendered.
#'  Basically, this callback will be called in each time when the chart is redrawed.
#' @param onresize Set a callback to execute when user resizes the screen.
#' @param onresized Set a callback to execute when screen resize finished.
#' 
#' @note Set JavaScript callbacks for various billboard events.
#'  See the \href{https://naver.github.io/billboard.js/release/latest/doc/Options.html}{billboard options} 
#'  reference for additional details on the signature of each callback.
#'
#' @return A \code{billboard} \code{htmlwidget} object.
#' @export
#' 
#' @importFrom htmlwidgets JS
#' @importFrom stats setNames
#' @importFrom utils modifyList
#' 
bb_callbacks <- function(bb, onafterinit = NULL, onbeforeinit = NULL,
                         oninit = NULL, onout = NULL, onover= NULL, 
                         onrendered = NULL, onresize = NULL, onresized = NULL) {
  
  if(!any(class(bb) %in% c("billboarder", "billboarder_Proxy"))){
    stop("bb must be a billboarder or a billboarderProxy object")
  }
  
  callbacks <- dropNulls(list(
    onafterinit = onafterinit,
    onbeforeinit  = onbeforeinit,
    oninit = oninit,
    onout  = onout,
    onover = onover,
    onrendered = onrendered,
    onresize = onresize,
    onresized = onresized
  ))
  
  callbacks <- lapply(
    X = setNames(callbacks, names(callbacks)),
    FUN = function(x) {
      if (!"JS_EVAL" %in% class(x)) {
        JS(x)
      } else {
        x
      }
    }
  )
  
  bb$x$bb_opts <- modifyList(x = bb$x$bb_opts, val = callbacks)
  return(bb)
}





