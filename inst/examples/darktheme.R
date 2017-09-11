


# dark theme --------------------------------------------------------------



billboarder() %>% 
  bb_barchart(data = table(sample(letters[1:5], 20, TRUE)), color = "#FFF") %>% 
  bb_add_style(
    "body" = "background-color: #000 !important;",
    # ".bb-axis" = "stroke: white; fill: white;"
    ".bb" = "fill: #FFF;", 
    ".bb-axis-x path, .bb-axis-x line" = "stroke: #FFF;",
    ".bb-axis-y path, .bb-axis-y line" = "stroke: #FFF;"
  )


data("economics", package = "ggplot2")

billboarder() %>%
  bb_linechart(data = economics[, c("date", "psavert")]) %>% 
  bb_x_axis(tick = list(format = "%Y-%m", fit = FALSE)) %>%
  bb_y_axis(tick = list(format = suffix("%")), 
            label = list(text = "Personal savings rate")) %>% 
  bb_legend(show = FALSE) %>% 
  bb_x_grid(show = TRUE) %>% 
  bb_y_grid(show = TRUE) %>% 
  bb_subchart(show = TRUE) %>% 
  bb_labs(title = "Title") %>% 
  bb_add_style(
    "body" = "background-color: #0f0f0f !important;",
    # ".bb-axis" = "stroke: white; fill: white;"
    ".bb" = "fill: whitesmoke;", 
    ".bb-axis-x path, .bb-axis-x line" = "stroke: whitesmoke;",
    ".bb-axis-y path, .bb-axis-y line" = "stroke: whitesmoke;"
  )



".inverted .bb .bb-axis-x path,
.inverted .bb .bb-axis-x line,
.inverted .bb .bb-axis-y path,
.inverted .bb .bb-axis-y line" = "stroke: white;",
".inverted .bb .bb-axis-x g,
.inverted .bb .bb-axis-y g,
    .inverted .bb .bb-legend-item-data text" = "fill: whitesmoke;"

# .inverted .bb .bb-axis-x path,
# .inverted .bb .bb-axis-x line,
# .inverted .bb .bb-axis-y path,
# .inverted .bb .bb-axis-y line {
#   stroke: white;
# }
# 
# .inverted .bb .bb-axis-x g,
# .inverted .bb .bb-axis-y g,
# .inverted .bb .bb-legend-item-data text {
#   fill: whitesmoke;
# }




# fun ----
bb_dark_theme <- function(bb) {
  bb_add_style(
    bb = bb,
    "body" = "background-color: #000 !important;",
    ".bb" = "fill: #FFF;", 
    ".bb-axis-x path, .bb-axis-x line" = "stroke: #FFF;",
    ".bb-axis-y path, .bb-axis-y line" = "stroke: #FFF;"
  )
}




# tests ----

library("billboarder")


## bars

stars <- data.frame(
  package = c("billboarder", "ggiraph", "officer", "shinyWidgets", "visNetwork"),
  stars = c(1, 176, 42, 40, 166)
)

billboarder() %>%
  bb_barchart(data = stars, labels = TRUE) %>%
  bb_data(names = list(stars = "Number of stars")) %>% 
  bb_axis(rotated = TRUE) %>% 
  bb_dark_theme()



## pie

billboarder() %>% 
  bb_piechart(data = stars) %>% 
  bb_dark_theme()



# line
AirPassengers1960 <- data.frame(
  month = month.name, 
  AirPassengers = tail(AirPassengers, 12)
)

billboarder() %>% 
  bb_linechart(data = AirPassengers1960, x = "month") %>% 
  bb_x_axis(type = "category") %>% 
  bb_dark_theme()


data("economics", package = "ggplot2")
billboarder() %>%
  bb_linechart(data = economics[, c("date", "psavert")]) %>% 
  bb_x_axis(tick = list(format = "%Y-%m", fit = FALSE)) %>%
  bb_y_axis(tick = list(format = suffix("%")), 
            label = list(text = "Personal savings rate")) %>% 
  bb_legend(show = FALSE) %>% 
  bb_x_grid(show = TRUE) %>% 
  bb_y_grid(show = TRUE) %>% 
  bb_subchart(show = TRUE) %>% 
  bb_dark_theme()




# density
data("diamonds", package = "ggplot2")
billboarder() %>% 
  bb_densityplot(data = diamonds, x = "depth", group = "cut") %>% 
  bb_x_axis(min = 55, max = 70) %>% 
  bb_dark_theme()







