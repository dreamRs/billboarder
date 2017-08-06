

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









