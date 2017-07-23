


# bar chart example -------------------------------------------------------


# Packages ----

library("billboarder")
library("data.table")
library("magrittr")



# data ----

data("mpg", package = "ggplot2")
setDT(mpg)
data("diamonds", package = "ggplot2")
setDT(diamonds)





# ex1 : simple bar --------------------------------------------------------


billboarder() %>%
  bb_bar(data = mpg[, list(n = .N), by = class])

billboarder() %>%
  bb_bar(data = mpg[, list(n = .N), by = class]) %>%
  bb_axis(rotated = TRUE)





# ex2 : stacked bar -------------------------------------------------------



mpg[, list(n = .N), by = list(class, drv)]
dcast(data = mpg[, list(n = .N), by = list(class, drv)], formula = class~drv, value.var = "n")

billboarder() %>%
  bb_bar(data = dcast(data = mpg[, list(n = .N), by = list(class, drv)], formula = class~drv, value.var = "n"))

billboarder() %>%
  bb_bar(data = dcast(data = mpg[, list(n = .N), by = list(class, drv)], formula = class~drv, value.var = "n"), stacked = TRUE)


# params <- list(
#   data = list(
#     json = list(
#       data1 = c(10, 50, 30, 80),
#       data2 = c(30, 10, 40, 0),
#       data3 = c(50, 30, 10, 20)
#     ),
#     groups = list(list("data1", "data2", "data3")),
#     type = "bar"
#   )
# )
# billboarder(params)

