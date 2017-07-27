


# README ------------------------------------------------------------------




# packages ----------------------------------------------------------------

library("billboarder")
library("magrittr")
library("data.table")
library("dplyr")
library("tidyr")




# data --------------------------------------------------------------------

data("mpg", package = "ggplot2")
setDT(mpg)

data(iris)
data(mtcars)




# bar ---------------------------------------------------------------------


### simple bar chart

billboarder() %>%
  bb_bar(data = mpg[, list(count = .N), by = manufacturer][order(count)]) %>%
  bb_axis(rotated = TRUE) %>%
  bb_title(text = "Number of models by manufacturer", position = "left-top")


# With dplyr
mpg %>% 
  count(manufacturer) %>% 
  arrange(n) %>% 
  billboarder(data = .) %>% 
  bb_bar() %>%
  bb_axis(rotated = TRUE) %>%
  bb_title(text = "Number of models by manufacturer", position = "left-top")




### Stacked and dodge

billboarder() %>%
  bb_bar(
    data = dcast(
      data = mpg[, list(count = .N), by = list(manufacturer, year)],
      formula = manufacturer~year,
      value.var = "count"
    )
  )


mpg %>% 
  group_by(manufacturer, year) %>% 
  summarise(n = n()) %>% 
  spread(year, n) %>% 
  billboarder(data = .) %>%
  bb_bar(stacked = TRUE, labels = TRUE)







# Points ------------------------------------------------------------------

billboarder() %>% 
  bb_scatter(data = iris, x = "Sepal.Length", y = "Sepal.Width")


billboarder() %>% 
  bb_scatter(data = iris, x = "Sepal.Length", y = "Sepal.Width", group = "Species")


billboarder() %>% 
  bb_scatter(data = mtcars, x = "wt", y = "mpg", group = "cyl")

billboarder() %>% 
  bb_scatter(data = mtcars, x = "wt", y = "mpg", group = "cyl") %>% 
  bb_axis(x = list(tick = list(fit = FALSE)))





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

