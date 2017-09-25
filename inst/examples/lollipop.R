

#  ------------------------------------------------------------------------
#
# Title : Lollipop
#    By : VP
#  Date : 2017-08-24
#    
#  ------------------------------------------------------------------------



# Packages ----------------------------------------------------------------

library("billboarder")



# Data --------------------------------------------------------------------

# From wikipedia
sw <- data.frame(
  film = c("The Force Awakens", "The Phantom Menace", "Revenge of the Sith",
           "A New Hope", "Attack of the Clones", "The Empire Strikes Back", "Return of the Jedi"
  ),
  worldwide_gross = c(2068178225, 1027044677, 848754768,
                      775398007, 649398328, 538375067, 475106177)
)



# Lollipop ----------------------------------------------------------------


# raw
lll <- billboarder() %>% 
  bb_barchart(data = cbind(sw, lollipop = sw$worldwide_gross), rotated = FALSE) %>% 
  bb_data(classes = list(worldwide_gross = "lollipop-lines")) %>% 
  bb_bar(width = 0.05) %>% 
  bb_x_axis(tick = list(centered = TRUE)) %>% 
  bb_data(types = list(lollipop = "bar", worldwide_gross = "line")) %>% 
  bb_colors_manual(list(lollipop = "#000", worldwide_gross = "firebrick")) %>%
  bb_point(r = 8) %>% 
  bb_add_style(".bb-circle-worldwide-gross" = "opacity: 1;", ".bb-target-lollipop-lines > .bb-lines" = "opacity: 0;") %>% 
  bb_legend(hide = TRUE) %>% 
  bb_tooltip(
    format = list(
      value = htmlwidgets::JS("function(value, ratio, id, index) {if (id !== 'lollipop') return value; }")
    )
  )

lll


# prettier
lll %>% 
  bb_y_grid(show = TRUE) %>% 
  bb_y_axis(tick = list(
    values = c(0, 5e+08, 1e+09, 1.5e+09, 2e+09),
    outer = FALSE,
    format = htmlwidgets::JS("d3.formatPrefix('$,.0', 1e6)")
  )) %>% 
  bb_labs(
    title = "Star Wars - Total Lifetime Grosses",
    caption = "Data source : wikipedia"
  )


# 




# Test fun ----------------------------------------------------------------

billboarder() %>% 
  bb_lollipop(data = sw)




# Fun ---------------------------------------------------------------------



bb_lollipop <- function(bb, data, x = NULL, y = NULL, rotated = FALSE, point_color = "#112446", point_size = 8, line_color = "#000", ...) {
  
  if (missing(data))
    data <- bb$x$data
  
  data <- as.data.frame(data)
  args <- list(...)
  
  if (is.null(x))
    x <- names(data)[1]
  
  if (is.null(y))
    y <- names(data)[2]
  
  data <- data[c(x, y)]
  data <- cbind(data, lollipop = data[[y]])
  
  if (nrow(data) == 1) {
    json <- lapply(X = as.list(data), FUN = list)
  } else {
    json <- as.list(data)
  }
  
  data_opt <- list(
    x = x,
    json = json,
    type = "bar",
    classes = stats::setNames(list("lollipop-lines"), y),
    types = stats::setNames(c(list("bar", "line")), c("lollipop", y)),
    colors = stats::setNames(c(list(line_color, point_color)), c("lollipop", y))
  )
  
  bb <- .bb_opt2(bb, "data", data_opt)
  
  bb <- .bb_opt(bb, "axis", x = list(type = "category"), rotated = rotated)
  
  bb <- .bb_opt(bb, "bar", width = 0.05)

  bb <- .bb_opt(bb, "point", r = point_size)
  
  bb <- bb_add_style(
    bb = bb, 
    ".bb-target-lollipop-lines > .bb-circle" = "opacity: 1;", 
    ".bb-target-lollipop-lines > .bb-lines" = "opacity: 0;"
  )
  
  bb <- .bb_opt(bb, format = list(
    value = htmlwidgets::JS("function(value, ratio, id, index) {if (id !== 'lollipop') return value; }")
  ))
  
  return(bb)
}



