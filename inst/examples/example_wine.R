

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




# Imports / Exports France ------------------------------------------------

balance_fr <- eu_wine %>% 
  filter(prod_bal == "Wine - Total",
         bal_item == "Total exports (for EUR : Exports to third countries) (1000 hl)" |
           bal_item == "Total imports (for EUR : imports from third countries) (1000 hl)",
         geo == "France") %>% 
  mutate(type = if_else(grepl("imports", bal_item), "imports", "exports")) %>% 
  select(time, type, values) %>% 
  spread(type, values) %>% 
  arrange(time) %>% 
  mutate(balance = exports - imports)

billboarder() %>% 
  bb_barchart(data = balance_fr %>% select(time, balance)) %>% 
  bb_y_grid(show = TRUE) %>% 
  bb_x_axis(tick = list(fit = FALSE)) %>% 
  bb_legend(show = FALSE) %>% 
  bb_labs(title = "Wine imports and exports in France", 
          y = "Exports minus Imports",
          caption = "Data source: Eurostats")






# Red and White wine ------------------------------------------------------

red_and_white_fr <- eu_wine %>% 
  filter(
    bal_item=="Official production (1000 hl)",
    prod_bal %in% c("Red and rose wine", "White wine"),
    geo == "France", time == 1988
  )


billboarder() %>% 
  bb_piechart(data = red_and_white_fr %>% select(prod_bal, values))



red_and_white_it <- eu_wine %>% 
  filter(
    bal_item=="Official production (1000 hl)",
    # prod_bal %in% c("Red and rose wine", "White wine"),
    geo == "Italy"#, time == 2015
  )


billboarder() %>% 
  bb_piechart(data = red_and_white_it %>% select(prod_bal, values))





red_and_white_uk <- eu_wine %>% 
  filter(
    bal_item=="Gross human consumption (1000 hl)",
    # prod_bal %in% c("Red and rose wine", "White wine"),
    geo == "United Kingdom"#, time == 2015
  )


billboarder() %>% 
  bb_piechart(data = red_and_white_uk %>% select(prod_bal, values))








# Part of PDO and PGI in France -------------------------------------------


labeled2016_fr <- eu_wine %>% 
  filter(
    bal_item=="Official production (1000 hl)",
    prod_bal %in% c("P.G.I. Total wine", "P.D.O. Total wine", "Varietal wine - Total", "Other wine - Total"),
    geo == "France", time == 2016
  ) %>% 
  mutate(label = if_else(grepl("P\\.D\\.O\\.|P\\.G\\.I\\.", prod_bal), "With EU label", "No label")) %>% 
  group_by(label) %>% 
  summarise(values = sum(values))


billboarder() %>% 
  bb_donutchart(data = labeled2016_fr) %>% 
  bb_donut(title = "2016")


labeled2010_fr <- eu_wine %>% 
  filter(
    bal_item=="Official production (1000 hl)",
    prod_bal %in% c("P.G.I. Total wine", "P.D.O. Total wine", "Varietal wine - Total", "Other wine - Total"),
    geo == "France", time == 2010
  ) %>% 
  mutate(label = if_else(grepl("P\\.D\\.O\\.|P\\.G\\.I\\.", prod_bal), "With EU label", "No label")) %>% 
  group_by(label) %>% 
  summarise(values = sum(values))


billboarder() %>% 
  bb_donutchart(data = labeled2010_fr) %>% 
  bb_donut(title = "2010")





eu_wine %>% 
  filter(
    bal_item=="Official production (1000 hl)",
    prod_bal %in% c("P.G.I. Total wine", "P.D.O. Total wine", "Varietal wine - Total", "Other wine - Total"),
    geo == "Italy"
  ) %>% 
  mutate(label = if_else(grepl("P\\.D\\.O\\.|P\\.G\\.I\\.", prod_bal), "With EU label", "No label")) %>% 
  group_by(time, label) %>% 
  summarise(values = sum(values))








labeled2016_it <- eu_wine %>% 
  filter(
    bal_item=="Official production (1000 hl)",
    prod_bal %in% c("P.G.I. Total wine", "P.D.O. Total wine", "Varietal wine - Total", "Other wine - Total"),
    geo == "Italy", time == 2016
  ) %>% 
  mutate(label = if_else(grepl("P\\.D\\.O\\.|P\\.G\\.I\\.", prod_bal), "With EU label", "No label")) %>% 
  group_by(label) %>% 
  summarise(values = sum(values))


billboarder() %>% 
  bb_donutchart(data = labeled2016_it) %>% 
  bb_donut(title = "2016")


labeled2010_it <- eu_wine %>% 
  filter(
    bal_item=="Official production (1000 hl)",
    prod_bal %in% c("P.G.I. Total wine", "P.D.O. Total wine", "Varietal wine - Total", "Other wine - Total"),
    geo == "Italy", time == 2010
  ) %>% 
  mutate(label = if_else(grepl("P\\.D\\.O\\.|P\\.G\\.I\\.", prod_bal), "With EU label", "No label")) %>% 
  group_by(label) %>% 
  summarise(values = sum(values))


billboarder() %>% 
  bb_donutchart(data = labeled2010_it) %>% 
  bb_donut(title = "2010")










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





# PDO (AOC) et PGI (VDP) --------------------------------------------------


pgi <- eu_wine %>% 
  filter(geo %in% c("France")) %>% 
  filter(prod_bal %in% c("P.G.I. - Red and rose wine", "P.G.I. - white wine")) %>% 
  filter(bal_item == "Official production (1000 hl)") %>% 
  mutate(type = if_else(grepl("white", prod_bal), "White wine", "Red and rose wine")) %>% 
  select(time, type, values) %>% 
  spread(type, values)
  
billboarder() %>% 
  bb_barchart(data = pgi)

billboarder() %>% 
  bb_linechart(data = pgi, type = "area", x = "time") %>% 
  bb_data(groups = list(list("White wine", "Red and rose wine")))



pdo <- eu_wine %>% 
  filter(geo %in% c("France")) %>% 
  filter(prod_bal %in% c("P.D.O. - Red and rose wine", "P.D.O. -  white wine")) %>% 
  filter(bal_item == "Official production (1000 hl)") %>% 
  mutate(type = if_else(grepl("white", prod_bal), "White wine", "Red and rose wine")) %>% 
  select(time, type, values) %>% 
  spread(type, values)

pdo

billboarder() %>% 
  bb_linechart(data = pdo, type = "area", x = "time") %>% 
  bb_data(groups = list(list("White wine", "Red and rose wine")))





# PDO (AOC) in France, Spain and Italy ------------------------------------


pdo_frspit <- eu_wine %>% 
  filter(geo %in% c("France", "Spain", "Italy")) %>% 
  filter(prod_bal %in% c("P.D.O. Total wine")) %>% 
  filter(bal_item == "Official production (1000 hl)") %>% 
  select(time, geo, values) %>% 
  spread(geo, values)

billboarder() %>% 
  bb_barchart(data = pdo_frspit) %>% 
  bb_colors_manual("France" = "#00267F", "Italy" = "#009246", "Spain" = "#C60B1E") %>% 
  bb_data(labels = TRUE) %>% 
  bb_y_grid(show = TRUE) %>% 
  bb_legend(position = "right") %>% 
  bb_labs(title = "PDO (AOC) production in France, Spain and Italy", 
          y = "Official production (1000 hl)", 
          caption = "Data source: Eurostats")








# Gross human consumption -------------------------------------------------


ghc <- eu_wine %>% 
  filter(time == 2016) %>% 
  filter(prod_bal %in% c("P.D.O. - Red and rose wine", "P.D.O. -  white wine")) %>% 
  filter(bal_item == "Gross human consumption (1000 hl)") %>% 
  mutate(geo = gsub(" \\(.*", "", geo)) %>% 
  mutate(type = if_else(grepl("white", prod_bal), "White wine", "Red and rose wine")) %>% 
  select(geo, type, values) %>% 
  spread(type, values) %>% 
  arrange(`Red and rose wine` + `White wine`)
ghc


billboarder() %>% 
  bb_barchart(data = ghc, stacked = TRUE, rotated = TRUE) %>% 
  bb_color(palette = c("#A74947", "#D1C659")) %>% 
  bb_y_grid(show = TRUE) %>% 
  bb_labs(title = "Gross human consumption of P.D.O. wines",
          y = "In 1000hl",
          caption = "Data source: Eurostats")





# Gross human consumption per capita (lt/head) ----------------------------

ghcpc <- eu_wine %>% 
  filter(time == 2015) %>%
  filter(prod_bal %in% c("Red and rose wine", "White wine")) %>%
  filter(bal_item == "Gross human consumption per capita (lt/head)") %>% 
  mutate(geo = gsub(" \\(.*", "", geo)) %>% 
  select(geo, prod_bal, values) %>% 
  spread(prod_bal, values) %>% 
  arrange(`Red and rose wine` + `White wine`)



billboarder() %>% 
  bb_barchart(data = ghcpc, stacked = TRUE, rotated = FALSE) %>% 
  bb_colors_manual("Red and rose wine" = "#A74947", "White wine" = "#D1C659") %>% 
  bb_y_grid(show = TRUE) %>% 
  bb_labs(title = "Gross human consumption per capita",
          y = "In lt/head",
          caption = "Data source: Eurostats")





# Europe exports ----------------------------------------------------------


eu_balance <- eu_wine %>% 
  # filter(time == 2016) %>% 
  # filter(prod_bal %in% c("P.D.O. - Red and rose wine", "P.D.O. -  white wine")) %>% 
  filter(geo == "European Union (15 countries)")






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




