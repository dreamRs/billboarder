

# histogram ---------------------------------------------------------------



# dev ---------------------------------------------------------------------


library(ggplot2)
library("billboarder")


# One var ----

h <- graphics::hist(x = ggplot2::diamonds$price, breaks = 30, plot = FALSE)
h <- graphics::hist(x = iris$Sepal.Length, breaks = "FD", plot = FALSE)
h

h.df <- data.frame(
  data = h$counts,
  # x = paste(head(h$breaks, -1), tail(h$breaks, -1), sep = " - ")
  x = h$mids
)

# diff_break <- diff(h$breaks)[1]/2

# with bar chart
library(billboarder)
billboarder() %>% 
  bb_data(data = h.df, x = "x", type = "bar") %>% 
  bb_bar(width = list(ratio = 1)) %>%
  bb_x_axis(type = "category", tick = list(fit = FALSE, centered = TRUE)) %>%
  bb_tooltip(
    format = list(
      title = htmlwidgets::JS(
        paste(
          "function(i) {",
          paste0("var categories = ", jsonlite::toJSON(x = paste(head(h$breaks, -1), tail(h$breaks, -1), sep = " - ")), ";"),
          "return categories[i];",
          "}", collapse = "\n"
        )
      )
    )
  )



# with area step

diff_break <- diff(h$breaks)[1]/2
ticks_values <- scales::pretty_breaks(5)(h.df[[2]])
max_x_axis <- max(ticks_values)
padding_x <- max_x_axis / 25

billboarder() %>% 
  # billboarder:::bb_histogram(x = ggplot2::diamonds$price, breaks = 30) %>% 
  bb_data(data = h.df, x = "x", type = "area-step") %>% 
  bb_line(step = list(type = "step")) %>% 
  bb_x_axis(tick = list(fit = FALSE, outer = FALSE, values = ticks_values), max = max_x_axis, padding = list(right = padding_x)) %>%
  # bb_x_axis(type = "category", tick = list(fit = FALSE, outer = FALSE, center = TRUE)) %>% 
  bb_colors_manual(opacity = 1) %>%
  bb_tooltip(
    format = list(
      title = htmlwidgets::JS(
        paste(
          "function(i) {",
          paste0("var x = i - ", diff_break, " + ' - ' + (i + ", diff_break,");"),
          "return x;",
          "}", collapse = "\n"
        )
      )
    )
  )




# go with this (tooltip to made)

p <- ggplot(data = diamonds) + geom_histogram(mapping = aes(x = price), bins = 20) # , binwidth = 500
# dat <- ggplot_build(p)$data[[1]]
dat <- layer_data(p, i = 1L)
dat
p

dat$x <- round(dat$x)
diff_break <- diff(dat$x)[1]/2
ticks_values <- scales::pretty_breaks(5)(dat$x)
max_x_axis <- max(ticks_values)
padding_x <- max_x_axis / 25

billboarder() %>% 
  bb_data(data = dat[, c("x", "y")], x = "x", type = "area-step") %>% 
  # bb_line(step = list(type = "step")) %>%
  # bb_x_axis(tick = list(fit = FALSE, outer = FALSE, values = ticks_values), max = max_x_axis, padding = list(right = padding_x)) %>%
  bb_x_axis(type = "category", tick = list(fit = FALSE, outer = FALSE, centered = TRUE)) %>% 
  bb_colors_manual(opacity = 1) %>%
  bb_tooltip(
    format = list(
      title = htmlwidgets::JS(
        paste(
          "function(i) {",
          # paste0("var x = i - ", diff_break, " + ' - ' + (i + ", diff_break,");"),
          # "return x;",
          paste0("var categories = ", jsonlite::toJSON(x = paste(dat$xmin, dat$xmax, sep = " - ")), ";"),
          "return categories[i];",
          "}", collapse = "\n"
        )
      )
    )
  )



# ticks_values <- which(h.df[[2]] %in% scales::pretty_breaks(5)(h.df[[2]]))
# 
# billboarder() %>% 
#   bb_data(data = h.df[, 1, drop = FALSE], type = "bar") %>% 
#   bb_bar(width = list(ratio = 1)) %>%
#   bb_x_axis(type = "category", categories = h.df[[2]], tick = list(fit = FALSE, values = )) %>%
#   bb_tooltip(
#     format = list(
#       title = htmlwidgets::JS(
#         paste(
#           "function(i) {",
#           paste0("var categories = ", jsonlite::toJSON(x = paste(head(h$breaks, -1), tail(h$breaks, -1), sep = " - ")), ";"),
#           "return categories[i];",
#           "}", collapse = "\n"
#         )
#       )
#     )
#   )



#
library(highcharter)
hchist(ggplot2::diamonds$price)

library(ggplot2)
p <- ggplot(data = diamonds) + geom_histogram(mapping = aes(x = price))
p

ggplot_build(p)$data[[1]]
layer_data(p, i = 1L)

p <- ggplot(data = diamonds) + geom_histogram(mapping = aes(x = price, fill = cut, text = cut)) 
# p

ggplot_build(p)$data[[1]]






# grouping var ----




diamonds$cut <- as.character(diamonds$cut)
range_bins <- range(diamonds$price)
lh <- lapply(
  X = unique(diamonds$cut),
  FUN = function(x) {
    h <- graphics::hist(x = diamonds$price[diamonds$cut == x],plot = FALSE, breaks = 30)
    data.frame(count = h$counts, x = h$mids, group = x, stringsAsFactors = FALSE)
  }
)

str(lh)

lhdf <- do.call("rbind", lh)
head(lhdf)

lhdfr <- reshape(data = lhdf, idvar = "x", timevar = "group", direction = "wide")


billboarder() %>% 
  bb_barchart(data = lhdfr, stacked = TRUE) %>% 
  bb_x_axis(tick = list(fit = FALSE))





dat <- diamonds

dat$breaks <- cut(x = dat$price, breaks = 30)
datagg <- aggregate(x = dat$breaks, by = list(cut = dat$cut, breaks = dat$breaks), FUN = length)
dataggr <- reshape(data = datagg, idvar = "breaks", timevar = "cut", direction = "wide")
  
billboarder() %>% 
  bb_barchart(data = dataggr, stacked = TRUE) %>% 
  bb_bar(width = list(ratio = 1)) %>%
  bb_x_axis(type = "category", tick = list(fit = FALSE, centered = TRUE))


adding_x <- max_x_axis / 25

billboarder() %>% 
  # billboarder:::bb_histogram(x = ggplot2::diamonds$price, breaks = 30) %>% 
  bb_data(data = dataggr, x = "breaks", type = "area-step",
          groups = list(list("x.Fair", "x.Good", "x.Ideal", "x.Premium", "x.Very Good"))) %>% 
  bb_line(step = list(type = "step")) %>% 
  bb_x_axis(type = "category", tick = list(fit = FALSE, outer = FALSE))
  

p <- ggplot(data = diamonds) + geom_histogram(mapping = aes(x = price, fill = cut, text = cut), binwidth = 500) 
head(ggplot_build(p)$data[[1]])

testgg <- reshape(data = layer_data(p, i = 1L)[, c("x", "count", "text")], idvar = "x", timevar = "text", direction = "wide")

testgg$x <- round(testgg$x)
ticks_values <- scales::pretty_breaks(5)(testgg$x)

billboarder() %>% 
  # billboarder:::bb_histogram(x = ggplot2::diamonds$price, breaks = 30) %>% 
  bb_data(data = testgg, x = "x", type = "area-step",
          groups = list(list("count.Very Good", "count.Premium", "count.Ideal", "count.Good", 
                             "count.Fair"))) %>% 
  bb_line(step = list(type = "step")) %>% 
  bb_x_axis(type = "category", tick = list(fit = FALSE, culling = list(max = 5), multiline = FALSE, centered = TRUE))



#  ------------------------------------------------------------------------



# examples ----

data("diamonds", package = "ggplot2")

# one variable
billboarder() %>% 
  bb_histogram(data = diamonds, x = "price")

# equivalent to
billboarder() %>% 
  bb_histogram(data = diamonds$price)





