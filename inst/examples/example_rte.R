

#  ------------------------------------------------------------------------
#
# Title : Example RTE
#    By : VP
#  Date : samedi 05/08/2017
#    
#  ------------------------------------------------------------------------



# Packages ----------------------------------------------------------------

library( billboarder )




# Datas -------------------------------------------------------------------

data("prod_par_filiere")
data("cdc_prod_filiere")




# Barcharts ---------------------------------------------------------------


# Simple

billboarder() %>%
  bb_barchart(data = prod_par_filiere[, c("annee", "prod_hydraulique")], color = "#102246") %>%
  bb_y_grid(show = TRUE) %>%
  bb_y_axis(tick = list(format = suffix("TWh")),
            label = list(text = "production (in terawatt-hours)", position = "outer-top")) %>% 
  bb_legend(show = FALSE) %>% 
  bb_labs(title = "French hydraulic production",
          caption = "Data source: RTE (https://opendata.rte-france.com)")




# Dodge

billboarder() %>%
  bb_barchart(data = prod_par_filiere[, c("annee", "prod_hydraulique", "prod_eolien", "prod_solaire")]) %>%
  bb_data(names = list(prod_hydraulique = "Hydraulic", prod_eolien = "Wind", prod_solaire = "Solar")) %>% 
  bb_y_grid(show = TRUE) %>%
  bb_y_axis(tick = list(format = suffix("TWh")),
            label = list(text = "production (in terawatt-hours)", position = "outer-top")) %>% 
  bb_legend(position = "inset", inset = list(anchor = "top-right")) %>% 
  bb_labs(title = "Renewable energy production",
          caption = "Data source: RTE (https://opendata.rte-france.com)")




# Stacked

billboarder() %>%
  bb_barchart(data = prod_par_filiere[, c("annee", "prod_hydraulique", "prod_eolien", "prod_solaire")], stacked = TRUE) %>%
  bb_data(names = list(prod_hydraulique = "Hydraulic", prod_eolien = "Wind", prod_solaire = "Solar")) %>% 
  bb_y_grid(show = TRUE) %>%
  bb_y_axis(tick = list(format = suffix("TWh")),
            label = list(text = "production (in terawatt-hours)", position = "outer-top")) %>% 
  bb_legend(position = "inset", inset = list(anchor = "top-right")) %>% 
  bb_labs(title = "Renewable energy production",
          caption = "Data source: RTE (https://opendata.rte-france.com)")







# Piecharts ---------------------------------------------------------------

nuclear2016 <- data.frame(
  sources = c("Nuclear", "Other"),
  production = c(
    prod_par_filiere$prod_nucleaire[prod_par_filiere$annee == "2016"],
    prod_par_filiere$prod_total[prod_par_filiere$annee == "2016"] -
      prod_par_filiere$prod_nucleaire[prod_par_filiere$annee == "2016"]
  )
)


billboarder() %>% 
  bb_piechart(data = nuclear2016) %>% 
  bb_labs(title = "Share of nuclear power in France",
          caption = "Data source: RTE (https://opendata.rte-france.com)")







# Linecharts --------------------------------------------------------------



billboarder() %>% 
  bb_linechart(data = cdc_prod_filiere[, c("date_heure", "prod_eolien")])

billboarder() %>% 
  bb_linechart(data = cdc_prod_filiere[, c("date_heure", "prod_eolien")]) %>% 
  bb_x_axis(tick = list(format = "%H:%M", fit = FALSE))



billboarder() %>% 
  bb_linechart(data = cdc_prod_filiere[, c("date_heure", "prod_eolien", "prod_hydraulique", "prod_solaire")], type = "spline") %>% 
  bb_y_axis(min = 0, padding = 0)


billboarder() %>% 
  bb_linechart(
    data = cdc_prod_filiere[, c("date_heure", "prod_eolien", "prod_hydraulique", "prod_solaire")], 
    type = "area"
  ) %>% 
  bb_data(
    groups = list(list("prod_eolien", "prod_hydraulique", "prod_solaire")),
    names = list("prod_eolien" = "Wind", "prod_hydraulique" = "Hydraulic", "prod_solaire" = "Solar")
  ) %>% 
  bb_colors_manual("prod_eolien" = "#238443", "prod_hydraulique" = "#225EA8", "prod_solaire" = "#FEB24C", opacity = 1) %>% 
  bb_y_axis(min = 0, padding = 0)

# vs
billboarder() %>% 
  bb_linechart(
    data = cdc_prod_filiere[, c("date_heure", "prod_eolien", "prod_hydraulique", "prod_solaire")], 
    type = "area"
  ) %>% 
  bb_data(
    groups = list(list("prod_eolien", "prod_hydraulique", "prod_solaire")),
    colors = list("prod_eolien" = "#238443", "prod_hydraulique" = "#225EA8", "prod_solaire" = "#FEB24C"),
    names = list("prod_eolien" = "Wind", "prod_hydraulique" = "Hydraulic", "prod_solaire" = "Solar")
  ) %>% 
  bb_y_axis(min = 0, padding = 0)

