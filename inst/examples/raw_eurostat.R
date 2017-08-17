

library("eurostat")
library("dplyr")
library("tidyr")
library("billboarder")

dat <- get_eurostat(id = "tsdtr210", time_format = "num")
datl <- label_eurostat(dat)

trains <- datl %>% filter(vehicle == "Trains")

billboarder() %>% 
  bb_barchart(data = trains %>% filter(time == 1990) %>% select(geo, values))



billboarder() %>% 
  bb_barchart(data = trains %>% filter(time %in% c(1990, 2012)) %>% select(geo, time, values) %>% spread(time, values))




nrg_pc_204_c <- get_eurostat("nrg_pc_204_c")
nrg_pc_204_c <- label_eurostat(nrg_pc_204_c)

nrg_pc_204_c <- filter(nrg_pc_204_c, consom == "Band DA : Consumption < 1 000 kWh", breakdown == "Energy and supply")


billboarder() %>% 
  bb_linechart(data = nrg_pc_204_c %>% filter(geo == "France") %>% arrange(time) %>% select(time, values))




nrg_125m <- get_eurostat("nrg_125m", filters = list(geo = eu_countries$code))
nrg_125m <- filter(nrg_125m, geo %in% eu_countries$code)
nrg_125m <- label_eurostat(nrg_125m)
nrg_125m

billboarder() %>% 
  bb_linechart(data = nrg_125m %>% filter(geo == "France") %>% arrange(time) %>% select(time, values))




nrg_125a <- get_eurostat("nrg_125a")
nrg_125a <- filter(nrg_125a, geo %in% eu_countries$code)
nrg_125a <- label_eurostat(nrg_125a)
nrg_125a

billboarder() %>% 
  bb_linechart(data = nrg_125a %>%
                 filter(geo %in% c("France", "United Kingdom")) %>%
                 group_by(geo, time) %>% 
                 summarize(values = sum(values)) %>% 
                 spread(geo, values))

nrg_125a_a <- nrg_125a %>% 
  group_by(geo, time) %>% 
  summarize(values = sum(values))

billboarder() %>% 
  bb_barchart(data = nrg_125a_a %>% filter(lubridate::year(time) %in% c(1990, 2012)) %>% select(geo, time, values) %>% spread(time, values))






tsc00007 <- get_eurostat("tsc00007", time_format = "num", filters = list(geo = eu_countries$code))
tsc00007 <- label_eurostat(tsc00007)


billboarder() %>% 
  bb_barchart(data = tsc00007 %>%
                filter(time %in% c(2004, 2015)) %>%
                select(geo, time, values) %>% spread(time, values))

billboarder() %>% 
  bb_linechart(data = tsc00007 %>%
                filter(time %in% c(2004, 2015)) %>%
                select(geo, time, values) %>% spread(geo, values) %>% select(-time), show_point = TRUE) %>% 
  bb_x_axis(show = FALSE) %>% 
  bb_legend(show = FALSE) %>% 
  # bb_data(labels = list(format = list(setNames(lapply(eu_countries$code, function(x) htmlwidgets::JS(sprintf("function(v, id, i, j) {return '%s';}", x))), eu_countries$code)))) %>%
  # bb_data(labels = list(format = list(
  #   "UK" = htmlwidgets::JS("function(v, id, i, j) {return 'UK';}")
  # ))) %>% 
  bb_data(labels = list(format = htmlwidgets::JS("function(v, id, i, j) {return id;}"))) %>% 
  # bb_point(r = 6) %>% 
  bb_color(palette = "steelblue") %>% 
  bb_x_axis(padding = list(left = 0.1, right = 0.1)) %>% 
  bb_tooltip(grouped = FALSE)









# Wine --------------------------------------------------------------------


library("eurostat")
library("dplyr")
library("tidyr")
library("billboarder")
library("ggplot2")


eu_wine <- get_eurostat(id = "apro_cpb_wine", time_format = "num")
eu_wine <- label_eurostat(eu_wine)


eu_wine %>% 
  filter(prod_bal == "Wine - Total", bal_item == "Official production (1000 hl)", geo == "France") %>% 
  arrange(time) %>% 
  ggplot() + 
  aes(time, values) + 
  geom_line()



eu_wine %>% 
  filter(prod_bal == "Wine - Total", bal_item == "Official production (1000 hl)", geo == "France") %>% 
  arrange(time) %>% 
  select(time, values) %>% 
  billboarder(data = .) %>% 
  bb_linechart(type = "spline") %>% 
  bb_data(x = "time") %>% 
  bb_y_grid(show = TRUE) %>% 
  bb_x_grid(show = TRUE) %>% 
  bb_x_axis(tick = list(fit = FALSE)) %>% 
  bb_legend(show = FALSE) %>% 
  bb_labs(title = "Wine production in France", y = "Official production (1000 hl)", caption = "Data source: Eurostats")



