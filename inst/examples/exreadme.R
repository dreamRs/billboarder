


# README ------------------------------------------------------------------


library("billboarder")
library("magrittr")
library("data.table")




# bar ---------------------------------------------------------------------


# data
data("mpg", package = "ggplot2")
setDT(mpg)

# simple bar chart
billboarder() %>%
  bb_bar(data = mpg[, list(count = .N), by = manufacturer][order(count)]) %>%
  bb_axis(rotated = TRUE) %>%
  bb_title(text = "Number of models by manufacturer", position = "left-top")







# line --------------------------------------------------------------------

data(economics, package = "ggplot2")

params <- list(
  data = list(
    x = "x",
    json = list(
      x = economics$date,
      y = economics$psavert
    ),
    type = "spline"
  ),
  legend = list(show = FALSE),
  point = list(show = FALSE),
  axis = list(
    x = list(
      type = "timeseries",
      tick = list(
        count = 20,
        fit = TRUE,
        format = "%e %b %y"
      )
    ),
    y = list(
      label = list(
        text = "Personal savings rate"
      ),
      tick = list(
        format = htmlwidgets::JS("function(x) {return x + '%';}")
      )
    )
  )
)
billboarder(params)

