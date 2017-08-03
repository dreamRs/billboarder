


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

# with some other options
billboarder() %>%
  bb_bar(data = mpg[, list(count = .N), by = manufacturer][order(count, decreasing = TRUE)]) %>%
  bb_y_grid(show = TRUE) %>%
  bb_legend(show = FALSE) %>% 
  bb_y_axis(label = list(text = "# of models", position = "outer-top")) %>% 
  bb_title(text = "Popular models by manufacturer", position = "left-top", 
           padding = list(top = 0, right = 0, left = 0, bottom = 20))



# With dplyr
mpg %>% 
  count(manufacturer) %>% 
  arrange(n) %>% 
  billboarder(data = .) %>% 
  bb_bar() %>%
  bb_axis(rotated = TRUE) %>%
  bb_title(text = "Number of models by manufacturer", position = "left-top")




data("prod_par_filiere")

billboarder() %>%
  bb_bar(data = prod_par_filiere[, c("annee", "prod_hydraulique")]) %>%
  bb_color("#102246") %>% 
  bb_y_grid(show = TRUE) %>%
  bb_y_axis(tick = list(format = htmlwidgets::JS("function(x) {return x + 'TWh';}"))) %>% 
  bb_legend(show = FALSE) %>% 
  bb_title(text = "French hydraulic production", position = "right-top")

billboarder() %>%
  bb_bar(data = prod_par_filiere[, c("annee", "prod_hydraulique")]) %>%
  bb_color("#102246") %>% 
  bb_y_grid(show = TRUE) %>%
  bb_y_axis(tick = list(format = htmlwidgets::JS("function(x) {return x + 'TWh';}"))) %>% 
  bb_legend(show = FALSE) %>% 
  bb_labs(title = "French hydraulic production",
          caption = "Data source: RTE (https://opendata.rte-france.com)")


library("data.table")
data(prod_par_filiere)
setDT(prod_par_filiere)
prod_par_filiere <- melt(prod_par_filiere)
prod_par_filiere2016 <- prod_par_filiere[annee == "2016" & variable != "prod_total"]
prod_par_filiere2016 <- prod_par_filiere2016[, percent := round(value/sum(value)*100, 1)]
prod_par_filiere2016 <- prod_par_filiere2016[order(percent, decreasing = TRUE)]
prod_par_filiere2016 <- prod_par_filiere2016[, variable := gsub("prod_", "", variable)]
prod_par_filiere2016


billboarder() %>%
  bb_bar(data = prod_par_filiere2016[, c("variable", "percent")]) %>%
  bb_color("#102246") %>% 
  # bb_axis(rotated = TRUE) %>% 
  bb_data(labels = TRUE) %>% 
  bb_y_grid(show = TRUE) %>%
  bb_legend(show = FALSE) %>% 
  bb_labs(title = "French electricity production in 2016 by sources",
          y = "% of production",
          caption = "Data source: RTE (https://opendata.rte-france.com)")



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
  bb_bar(stacked = TRUE) %>% 
  bb_data(labels = TRUE)







# Points ------------------------------------------------------------------


# iris

billboarder() %>% 
  bb_scatter(data = iris, x = "Sepal.Length", y = "Sepal.Width")


billboarder() %>% 
  bb_scatter(data = iris, x = "Sepal.Length", y = "Sepal.Width", group = "Species") %>% 
  bb_axis(x = list(tick = list(fit = FALSE)))

billboarder() %>% 
 bb_scatter(data = iris, x = "Sepal.Length", y = "Sepal.Width", group = "Species") %>% 
 bb_axis(x = list(tick = list(fit = FALSE))) %>% 
 bb_point(r = 8)


# tooltip scatter
billboarder() %>% 
  bb_scatter(data = iris, x = "Sepal.Length", y = "Sepal.Width", group = "Species") %>% 
  bb_tooltip(
    format = list(
      # skip the title in tooltip
      title = htmlwidgets::JS("function() {return undefined;}"),
      name = htmlwidgets::JS("function(name, ratio, id, index) {return '';}"),
      value = htmlwidgets::JS("function(value, ratio, id, index) {return id;}")
    )
  )




# mtcars

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

