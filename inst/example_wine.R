

#  ------------------------------------------------------------------------
#
# Title : Wine
#    By : VP
#  Date : jeudi 17 août 2017
#    
#  ------------------------------------------------------------------------



# Packages ----------------------------------------------------------------

library( "billboarder" )
library( "eurostat" )
library( "dplyr" )
library( "tidyr" )




# Data --------------------------------------------------------------------

# also possible id : tag00034
eu_wine <- get_eurostat(id = "apro_cpb_wine", time_format = "num")
eu_wine <- label_eurostat(eu_wine)
eu_wine




# Top 10 productor in 2016 ------------------------------------------------

top_10 <- eu_wine %>% 
  filter(prod_bal == "Wine - Total",
         bal_item == "Official production (1000 hl)",
         time == "2016") %>% 
  top_n(n = 10, wt = values) %>% 
  mutate(geo = gsub(" \\(.*", "", geo)) %>% 
  arrange(desc(values)) %>% 
  select(geo, values)


billboarder() %>% 
  bb_barchart(data = top_10, rotated = TRUE) %>% 
  bb_data(names = list("values" = "Official production (1000 hl)")) %>%
  bb_y_grid(show = TRUE) %>%
  bb_y_axis(tick = list(values = seq(0, 5e4, 1e4))) %>% 
  bb_labs(title = "Top 10 wine producer in EU for 2016", caption = "Data source: Eurostats")




# Imports / Exports -------------------------------------------------------

imports_exports <- eu_wine %>% 
  filter(prod_bal == "Wine - Total",
         bal_item == "Total exports (for EUR : Exports to third countries) (1000 hl)" |
           bal_item == "Total imports (for EUR : imports from third countries) (1000 hl)",
         time == "2016") %>% 
  mutate(geo = gsub(" \\(.*", "", geo)) %>% 
  filter(geo %in% eu_countries$name) %>% 
  mutate(type = if_else(grepl("imports", bal_item), "imports", "exports"))


imports_exports <- imports_exports %>%
  select(geo, type, values) %>%
  spread(type, values) %>%
  # mutate(tmp = exports + imports) %>% 
  # top_n(n = 12, wt = tmp) %>% 
  mutate(bal_com = exports - imports) %>% 
  arrange(desc(bal_com)) %>% 
  select(-bal_com)


billboarder() %>% 
  bb_barchart(data = imports_exports) %>% 
  bb_y_grid(show = TRUE) %>%
  bb_data(label = TRUE) %>% 
  bb_labs(title = "Wine imports and exports for EU contries in 2016", 
          y = "(1000 hl)",
          caption = "Data source: Eurostats")


billboarder() %>% 
  bb_barchart(data = imports_exports %>% mutate(imports = -1 * imports), stacked = TRUE) %>% 
  bb_y_grid(show = TRUE) %>%
  bb_data(label = TRUE) %>% 
  bb_labs(title = "Wine imports and exports for EU contries in 2016", 
          y = "(1000 hl)",
          caption = "Data source: Eurostats")







# Red and White wine ------------------------------------------------------

red_and_white <- eu_wine %>% 
  filter(
    bal_item=="Official production (1000 hl)",
    prod_bal %in% c("Red and rose wine", "White wine"),
    geo == "France", time == 1988
  )


billboarder() %>% 
  bb_piechart(data = red_and_white %>% select(prod_bal, values))








# Wine production 1955 - 2016 ---------------------------------------------


wine_prod <- eu_wine %>% 
  filter(geo %in% c("France", "Italy", "Spain"),
         bal_item == "Official production (1000 hl)",
         prod_bal == "Wine - Total") %>% 
  select(geo, time, values) %>% 
  spread(geo, values) %>% 
  arrange(time)
  


billboarder() %>% 
  bb_linechart(data = wine_prod, type = "spline") %>% 
  bb_data(x = "time") %>% 
  bb_colors_manual("France" = "#00267F", "Italy" = "#009246", "Spain" = "#C60B1E") %>% 
  bb_data(color = htmlwidgets::JS("function(color, d) {return d3.rgb(color).brighter().toString();}")) %>% 
  bb_y_grid(show = TRUE) %>% 
  bb_x_grid(show = TRUE) %>% 
  bb_x_axis(tick = list(fit = FALSE)) %>% 
  bb_legend(position = "inset", inset = list(anchor = "top-right")) %>% 
  bb_labs(title = "Wine production for top 3 producer", 
          y = "Official production (1000 hl)", 
          caption = "Data source: Eurostats")








# garbage -----------------------------------------------------------------


eu_wine %>% 
  filter(geo == "European Union (27 countries)")



eu_wine %>% 
  filter(bal_item == "Official production (1000 hl)", geo == "France", time == "2016")



eu_wine %>% 
  filter(geo == "France") %>% 
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




