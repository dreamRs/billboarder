

context("Test readme")





# Tests -------------------------------------------------------------------


test_that("Bar chart", {
  
  # data
  data("prod_par_filiere")
  
  # a bar chart !
  bb <- billboarder() %>%
    bb_barchart(data = prod_par_filiere[, c("annee", "prod_hydraulique")], color = "#102246") %>%
    bb_y_grid(show = TRUE) %>%
    bb_y_axis(tick = list(format = suffix("TWh")),
              label = list(text = "production (in terawatt-hours)", position = "outer-top")) %>% 
    bb_legend(show = FALSE) %>% 
    bb_labs(title = "French hydraulic production",
            caption = "Data source: RTE (https://opendata.rte-france.com)")
  
  expect_silent(bb)
  expect_is(bb, "billboarder")
})


test_that("dodge bar chart", {

  # data
  data("prod_par_filiere")
  
  # dodge bar chart !
  bb <- billboarder() %>%
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
  
  expect_silent(bb)
  expect_is(bb, "billboarder")
})



test_that("stacked bar chart", {
  
  # data
  data("prod_par_filiere")
  
  # stacked bar chart !
  bb <- billboarder() %>%
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
  
  expect_silent(bb)
  expect_is(bb, "billboarder")
})


test_that("Scatter plot", {
  
  bb <- billboarder() %>% 
    bb_scatterplot(data = iris, x = "Sepal.Length", y = "Sepal.Width", group = "Species") %>% 
    bb_axis(x = list(tick = list(fit = FALSE))) %>% 
    bb_point(r = 8)
  
  expect_silent(bb)
  expect_is(bb, "billboarder")
})


test_that("Pie chart", {
  
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
  bb <- billboarder() %>% 
    bb_piechart(data = nuclear2016) %>% 
    bb_labs(title = "Share of nuclear power in France in 2016",
            caption = "Data source: RTE (https://opendata.rte-france.com)")
  
  expect_silent(bb)
  expect_is(bb, "billboarder")
})


test_that("Line chart Date", {
  
  # data
  data("equilibre_mensuel")
  
  # line chart
  bb <- billboarder() %>% 
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
            y = "In megawatt (MW)", x = "Date",
            caption = "Data source: RTE (https://opendata.rte-france.com)")
  
  expect_silent(bb)
  expect_is(bb, "billboarder")
})



test_that("Line chart POSIXct", {
  
  # data
  data("cdc_prod_filiere")
  
  # Retrieve sunrise and and sunset data with `suncalc`
  # library("suncalc")
  sun <- structure(list(sunrise = structure(1497239294,
                                            class = c("POSIXct", 
                                                      "POSIXt"), tzone = "CET"), 
                        sunset = structure(1497297332, class = c("POSIXct", 
                                                                 "POSIXt"), tzone = "CET")), 
                   .Names = c("sunrise", "sunset"), class = "data.frame", row.names = c(NA, 
                                                                                        -1L))
  
  
  # line chart
  bb <- billboarder() %>% 
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
            y = "In megawatt (MW)", x = "Time",
            caption = "Data source: RTE (https://opendata.rte-france.com)")
  
  expect_silent(bb)
  expect_is(bb, "billboarder")
})


test_that("Stacked area chart", {
  
  # data
  data("cdc_prod_filiere")
  
  # area chart !
  bb <- billboarder() %>% 
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
  
  expect_silent(bb)
  expect_is(bb, "billboarder")
})


test_that("RAW", {
  
  data(economics, package = "ggplot2")
  
  # Construct a list in JSON format
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
  
  # Pass the list as parameter
  bb <- billboarder(params)
  expect_silent(bb)
  expect_is(bb, "billboarder")
})





# Histogram & density -----------------------------------------------------



test_that("Histogram", {
  bb <- billboarder() %>%
    bb_histogram(data = rnorm(1e5), binwidth = 0.25) %>%
    bb_colors_manual()
  expect_silent(bb)
  expect_is(bb, "billboarder")
})

test_that("Histogram & density - grouped", {
  dat <- data.frame(
    sample = c(rnorm(n = 1e4, mean = 1), rnorm(n = 1e4, mean = 2)),
    group = rep(c("A", "B"), each = 1e4), stringsAsFactors = FALSE
  )
  # Mean by groups
  samples_mean <- tapply(dat$sample, dat$group, mean)
  # histogram !
  bb <- billboarder() %>%
    bb_histogram(data = dat, x = "sample", group = "group", binwidth = 0.25) %>%
    bb_x_grid(
      lines = list(
        list(value = unname(samples_mean['A']), text = "mean of sample A"),
        list(value = unname(samples_mean['B']), text = "mean of sample B")
      )
    )
  expect_silent(bb)
  expect_is(bb, "billboarder")
  
  bbd <- billboarder() %>%
    bb_densityplot(data = dat, x = "sample", group = "group") %>%
    bb_x_grid(
      lines = list(
        list(value = unname(samples_mean['A']), text = "mean of sample A"),
        list(value = unname(samples_mean['B']), text = "mean of sample B")
      )
    )
  expect_silent(bbd)
  expect_is(bbd, "billboarder")
})





# Lollipop ----------------------------------------------------------------

test_that("Lollipop", {
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
  bb <- billboarder() %>% 
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
  expect_silent(bb)
  expect_is(bb, "billboarder")
})




