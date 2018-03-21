

# Package -----------------------------------------------------------------

library(billboarder)





# Bubble ------------------------------------------------------------------


billboarder() %>% 
  bb_scatterplot(data = iris)


dat <- data.frame(x = c(1, 2, 3, 1, 2, 3), y = c(1, 2, 3, 2, 3, 4), z = c(5, 10, 15), group = c("a", "b", "c", "a", "b", "c"))
ggplot(dat) + geom_point(aes(x, y, color = group, size = z))

billboarder() %>% 
  bb_scatterplot(dat, bbaes(x, y))
billboarder() %>% 
  bb_scatterplot(dat, bbaes(x, y, size = z))
billboarder() %>% 
  bb_scatterplot(dat, bbaes(x, y, group = group, size = z))



# Explicit mapping
billboarder() %>% 
  bb_scatterplot(
    data = iris, 
    mapping = bbaes(Sepal.Length, Sepal.Width, size = Petal.Width)#, range = c(0.5, 120)
  ) %>% 
  # bb_bubble(maxR = 35) %>%
  bb_x_axis(tick = list(fit = FALSE))

billboarder() %>% 
  bb_scatterplot(
    data = iris, 
    mapping = bbaes(Sepal.Length, Sepal.Width, group = Species, size = Petal.Width),
    range = c(0.5, 120)
  ) %>% 
  # bb_bubble(maxR = 35) %>%
  bb_x_axis(tick = list(fit = FALSE))




# README ------------------------------------------------------------------



library("billboarder")

# data
data("prod_par_filiere")

# a bar chart !
billboarder() %>%
  bb_barchart(data = prod_par_filiere[, c("annee", "prod_hydraulique")], color = "#102246") %>%
  bb_y_grid(show = TRUE) %>%
  bb_y_axis(tick = list(format = suffix("TWh")),
            label = list(text = "production (in terawatt-hours)", position = "outer-top")) %>% 
  bb_legend(show = FALSE) %>% 
  bb_labs(title = "French hydraulic production",
          caption = "Data source: RTE (https://opendata.rte-france.com)")


library("billboarder")

# data
data("prod_par_filiere")

# dodge bar chart !
billboarder() %>%
  bb_barchart(
    data = prod_par_filiere[, c("annee", "prod_hydraulique", "prod_eolien", "prod_solaire")]
  ) %>%
  bb_data(
    names = list(prod_hydraulique = "Hydraulic", prod_eolien = "Wind", prod_solaire = "Solar")
  ) %>% 
  bb_y_grid(show = TRUE) %>%
  bb_y_axis(tick = list(format = suffix("TWh")),
            label = list(text = "production (in terawatt-hours)", position = "outer-top")) %>% 
  bb_legend(position = "inset", inset = list(anchor = "top-right")) %>% 
  bb_labs(title = "Renewable energy production",
          caption = "Data source: RTE (https://opendata.rte-france.com)")





library("billboarder")

# data
data("prod_par_filiere")

# stacked bar chart !
billboarder() %>%
  bb_barchart(
    data = prod_par_filiere[, c("annee", "prod_hydraulique", "prod_eolien", "prod_solaire")], 
    stacked = TRUE
  ) %>%
  bb_data(
    names = list(prod_hydraulique = "Hydraulic", prod_eolien = "Wind", prod_solaire = "Solar"), 
    labels = TRUE
  ) %>% 
  bb_colors_manual(
    "prod_eolien" = "#41AB5D", "prod_hydraulique" = "#4292C6", "prod_solaire" = "#FEB24C"
  ) %>%
  bb_y_grid(show = TRUE) %>%
  bb_y_axis(tick = list(format = suffix("TWh")),
            label = list(text = "production (in terawatt-hours)", position = "outer-top")) %>% 
  bb_legend(position = "right") %>% 
  bb_labs(title = "Renewable energy production",
          caption = "Data source: RTE (https://opendata.rte-france.com)")



billboarder() %>% 
  bb_scatterplot(data = iris, x = "Sepal.Length", y = "Sepal.Width", group = "Species") %>% 
  bb_axis(x = list(tick = list(fit = FALSE))) %>% 
  bb_point(r = 8)







library("billboarder")

# data
data("prod_par_filiere")
nuclear2016 <- data.frame(
  sources = c("Nuclear", "Other"),
  production = c(
    prod_par_filiere$prod_nucleaire[prod_par_filiere$annee == "2016"],
    prod_par_filiere$prod_total[prod_par_filiere$annee == "2016"] -
      prod_par_filiere$prod_nucleaire[prod_par_filiere$annee == "2016"]
  )
)

# pie chart !
billboarder() %>% 
  bb_piechart(data = nuclear2016) %>% 
  bb_labs(title = "Share of nuclear power in France in 2016",
          caption = "Data source: RTE (https://opendata.rte-france.com)")








library("billboarder")

# data
data("equilibre_mensuel")

# line chart
billboarder() %>% 
  bb_linechart(
    data = equilibre_mensuel[, c("date", "consommation", "production")], 
    type = "spline"
  ) %>% 
  bb_x_axis(tick = list(format = "%Y-%m", fit = FALSE)) %>% 
  bb_x_grid(show = TRUE) %>% 
  bb_y_grid(show = TRUE) %>% 
  bb_colors_manual("consommation" = "firebrick", "production" = "forestgreen") %>% 
  bb_legend(position = "right") %>% 
  bb_subchart(show = TRUE, size = list(height = 30)) %>% 
  bb_labs(title = "Monthly electricity consumption and production in France (2007 - 2017)",
          y = "In megawatt (MW)",
          caption = "Data source: RTE (https://opendata.rte-france.com)")



library("billboarder")

# data
data("cdc_prod_filiere")

# Retrieve sunrise and and sunset data with `suncalc`
library("suncalc")
sun <- getSunlightTimes(date = as.Date("2017-06-12"), lat = 48.86, lon = 2.34, tz = "CET")


# line chart
billboarder() %>% 
  bb_linechart(data = cdc_prod_filiere[, c("date_heure", "prod_solaire")]) %>% 
  bb_x_axis(tick = list(format = "%H:%M", fit = FALSE)) %>% 
  bb_y_axis(min = 0, padding = 0) %>% 
  bb_regions(
    list(
      start = as.numeric(cdc_prod_filiere$date_heure[1]) * 1000,
      end = as.numeric(sun$sunrise)*1000
    ), 
    list(
      start = as.numeric(sun$sunset) * 1000, 
      end = as.numeric(cdc_prod_filiere$date_heure[48]) * 1000
    )
  ) %>% 
  bb_x_grid(
    lines = list(
      list(value = as.numeric(sun$sunrise)*1000, text = "sunrise"),
      list(value = as.numeric(sun$sunset)*1000, text = "sunset")
    )
  ) %>% 
  bb_labs(title = "Solar production (2017-06-12)",
          y = "In megawatt (MW)",
          caption = "Data source: RTE (https://opendata.rte-france.com)")







library("billboarder")

# data
data("cdc_prod_filiere")

# area chart !
billboarder() %>% 
  bb_linechart(
    data = cdc_prod_filiere[, c("date_heure", "prod_eolien", "prod_hydraulique", "prod_solaire")], 
    type = "area"
  ) %>% 
  bb_data(
    groups = list(list("prod_eolien", "prod_hydraulique", "prod_solaire")),
    names = list("prod_eolien" = "Wind", "prod_hydraulique" = "Hydraulic", "prod_solaire" = "Solar")
  ) %>% 
  bb_legend(position = "inset", inset = list(anchor = "top-right")) %>% 
  bb_colors_manual(
    "prod_eolien" = "#238443", "prod_hydraulique" = "#225EA8", "prod_solaire" = "#FEB24C", 
    opacity = 0.8
  ) %>% 
  bb_y_axis(min = 0, padding = 0) %>% 
  bb_labs(title = "Renewable energy production (2017-06-12)",
          y = "In megawatt (MW)",
          caption = "Data source: RTE (https://opendata.rte-france.com)")




billboarder() %>%
  bb_histogram(data = rnorm(1e5), binwidth = 0.25) %>%
  bb_colors_manual()



# Generate some data
dat <- data.frame(
  sample = c(rnorm(n = 1e4, mean = 1), rnorm(n = 1e4, mean = 2)),
  group = rep(c("A", "B"), each = 1e4), stringsAsFactors = FALSE
)
# Mean by groups
samples_mean <- tapply(dat$sample, dat$group, mean)
# histogram !
billboarder() %>%
  bb_histogram(data = dat, x = "sample", group = "group", binwidth = 0.25) %>%
  bb_x_grid(
    lines = list(
      list(value = unname(samples_mean['A']), text = "mean of sample A"),
      list(value = unname(samples_mean['B']), text = "mean of sample B")
    )
  )



billboarder() %>%
  bb_densityplot(data = dat, x = "sample", group = "group") %>%
  bb_x_grid(
    lines = list(
      list(value = unname(samples_mean['A']), text = "mean of sample A"),
      list(value = unname(samples_mean['B']), text = "mean of sample B")
    )
  )



bb <- billboarder() %>% 
  bb_scatterplot(data = cars) %>% 
  bb_linechart(data = data.frame(lowess(cars)), x = "x") %>% 
  bb_data(types = list(dist = "scatter", y = "line"))

bb
bb$x$bb_opts


billboarder() %>% 
  bb_linechart(data = data.frame(lowess(cars)), x = "x") %>% 
  bb_scatterplot(data = cars) %>% 
  bb_data(types = list(dist = "scatter", y = "line"))





# Proxy -------------------------------------------------------------------


proxy_example("bar")
proxy_example("bar2")
proxy_example("line")
proxy_example("line2")
proxy_example("pie")
proxy_example("gauge")
proxy_example("density")
proxy_example("histogram")
proxy_example("lollipop")
proxy_example("stacked_bar")



# Check examples ----------------------------------------------------------


# example("bb_linechart", package = "billboarder", run.dontrun = TRUE)
# example("bb_densityplot", package = "billboarder", run.dontrun = TRUE)



# linechart ---------------------------------------------------------------

## Different types
x <- round(rnorm(20), 2)

billboarder() %>% 
  bb_linechart(data = x)

billboarder() %>% 
  bb_linechart(data = x, type = "spline")

billboarder() %>% 
  bb_linechart(data = x, type = "area")

billboarder() %>% 
  bb_linechart(data = x, type = "area-spline")


## Timeserie with date (Date)
data("economics", package = "ggplot2")

billboarder() %>%
  bb_linechart(data = economics[, c("date", "psavert")]) %>% 
  bb_x_axis(tick = list(format = "%Y-%m", fit = FALSE)) %>%
  bb_y_axis(tick = list(format = suffix("%")), 
            label = list(text = "Personal savings rate")) %>% 
  bb_legend(show = FALSE) %>% 
  bb_x_grid(show = TRUE) %>% 
  bb_y_grid(show = TRUE) %>% 
  bb_subchart(show = TRUE)


# With multiple lines :

data("economics_long", package = "ggplot2")

billboarder() %>%
  bb_linechart(economics_long, bbaes(date, value, variable)) %>% 
  bb_data(hide = "pop") %>% 
  bb_x_axis(tick = list(format = "%Y-%m", fit = FALSE))



## Timeserie with datetime (POSIXct)
data("cdc_prod_filiere")

billboarder() %>% 
  bb_linechart(data = cdc_prod_filiere[, c("date_heure", "prod_eolien")])

# or with mapping :
billboarder() %>% 
  bb_linechart(cdc_prod_filiere, bbaes(date_heure, prod_bioenergies))



### Other type for x-axis 

## character/factor on x-axis
AirPassengers1960 <- data.frame(
  month = month.name, 
  AirPassengers = tail(AirPassengers, 12)
)
# you have to specify that x-axis is of type 'category'
# and that column 'month' must be used for x-axis values
billboarder() %>% 
  bb_linechart(data = AirPassengers1960, x = "month") %>% 
  bb_x_axis(type = "category")


## numeric on x-axis
lynx.df <- data.frame(
  year = time(lynx),
  lynx = lynx
)
# just specify which variable must be use n the x-axis
billboarder() %>% 
  bb_linechart(data = lynx.df, x = "year")




df <- data.frame(
  cos = cos(seq(-pi, pi, length.out = 30))
)

# No legend
billboarder() %>% 
  bb_linechart(data = df) %>% 
  bb_legend(show = FALSE)


df2 <- data.frame(
  sin = sin(seq(-pi, pi, length.out = 30))
)

# No legend
billboarder() %>% 
  bb_linechart(data = df) %>% 
  bb_linechart(data = df2, type = "step")


# densityplot -------------------------------------------------------------

# With a vector
billboarder() %>%
  bb_densityplot(data = rnorm(1e4))

data("diamonds", package = "ggplot2")

# density plot with one variable
billboarder() %>% 
  bb_densityplot(data = diamonds, x = "carat")

# Same with mapping
billboarder() %>% 
  bb_densityplot(diamonds, bbaes(carat))

# With a grouping variable
billboarder() %>% 
  bb_densityplot(data = diamonds, x = "depth", group = "cut") %>% 
  bb_x_axis(min = 55, max = 70)

# Same with mapping
billboarder() %>% 
  bb_densityplot(diamonds, bbaes(depth, group = cut)) %>% 
  bb_x_axis(min = 55, max = 70)


# a stacked density plot using count as statistic
bb <- billboarder() %>%
  bb_densityplot(diamonds, bbaes(depth, group = cut),
                 stacked = TRUE, stat = "count") %>%
  bb_x_axis(min = 55, max = 70)
bb

# changing order
bb %>% bb_data(order = "asc")



# barchart ----------------------------------------------------------------

stars <- data.frame(
  package = c("billboarder", "ggiraph", "officer",
              "shinyWidgets", "visNetwork", "rAmCharts", 
              "D3partitionR"),
  stars = c(36, 194, 72, 61, 183, 25, 18)
)

# By default, first column is mapped on the x-axis
# second one on the y axis
billboarder() %>%
  bb_barchart(data = stars)


# Specify explicitly the columns to use
billboarder() %>%
  bb_barchart(data = stars, mapping = bbaes(package, stars), rotated = TRUE)


# Add some options
billboarder() %>%
  bb_barchart(data = stars[order(stars$stars), ], x = "package", y = "stars", rotated = TRUE) %>% 
  bb_data(names = list(stars = "Number of stars")) %>% 
  bb_y_grid(show = TRUE)



# Hack stacked barcharts (to color bar)
stars_wide <- data.frame(
  author = c("dreamRs", "davidgohel", "davidgohel", "dreamRs",
             "datastorm-open", "datastorm-open", "AntoineGuillot2"),
  package = c("billboarder", "ggiraph", "officer",
              "shinyWidgets", "visNetwork", "rAmCharts", 
              "D3partitionR"),
  stars = c(36, 194, 72, 61, 183, 25, 18)
)

billboarder() %>%
  bb_barchart(data = stars_wide, 
              mapping = bbaes(package, stars, group = author),
              stacked = TRUE)

billboarder() %>%
  bb_barchart(data = stars_wide,
              mapping = bbaes(author, stars, group = package),
              stacked = TRUE)



# Grouping variable
tab <- table(sample(letters[1:5], 100, TRUE), sample(LETTERS[1:5], 100, TRUE))
dat <- as.data.frame(tab)

billboarder() %>%
  bb_barchart(data = dat, bbaes(x = Var1, y = Freq, group = Var2), rotated = TRUE)


# You can also pass data in a 'wide' format
dat2 <- data.frame(
  x = letters[1:5],
  A = sample.int(n = 100, size = 5),
  B = sample.int(n = 100, size = 5),
  C = sample.int(n = 100, size = 5),
  D = sample.int(n = 100, size = 5),
  E = sample.int(n = 100, size = 5)
)

# But cannot use mapping
billboarder() %>%
  bb_barchart(data = dat2, stacked = TRUE) %>% 
  bb_data(order = NULL, labels = TRUE)








# Lollipop ----------------------------------------------------------------



# From wikipedia
sw <- data.frame(
  film = c("The Force Awakens", "The Phantom Menace", 
           "Revenge of the Sith", "A New Hope",
           "Attack of the Clones", "The Empire Strikes Back",
           "Return of the Jedi"
  ),
  worldwide_gross = c(2068178225, 1027044677, 848754768,
                      775398007, 649398328, 538375067,
                      475106177)
)

# Simple example
billboarder() %>% 
  bb_lollipop(data = sw)

# Fancy example
billboarder() %>% 
  bb_lollipop(data = sw, rotated = TRUE)%>% 
  bb_y_grid(show = TRUE) %>% 
  bb_y_axis(tick = list(
    values = c(0, 5e+08, 1e+09, 1.5e+09, 2e+09),
    outer = FALSE,
    format = htmlwidgets::JS("d3.formatPrefix('$,.0', 1e6)")
  )) %>% 
  bb_x_axis(tick = list(centered = TRUE)) %>% 
  bb_labs(
    title = "Star Wars - Total Lifetime Grosses",
    caption = "Data source : wikipedia"
  )


# With mapping
billboarder(data = sw) %>% 
  bb_lollipop(mapping = bbaes(x = film, y = worldwide_gross))

