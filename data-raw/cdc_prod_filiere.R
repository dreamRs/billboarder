
# Courbe de charge de la production d'électricite par filiere (janvier 2013 a juin 2017) -----------------
# Electricity generation by power source (january 2008 - june 2017)

# source RTE -  https://opendata.rte-france.com/explore/dataset/cdc_prod_par_filiere

# date extraction : 02/08/2017


library("jsonlite")

cdc_prod_filiere <- fromJSON(txt = "https://opendata.rte-france.com/api/records/1.0/search/?dataset=cdc_prod_par_filiere&q=date%3D%222017-06-12%22&rows=-1&sort=-date_heure")
str(cdc_prod_filiere, max.level = 2)

cdc_prod_filiere <- cdc_prod_filiere$records$fields

library("data.table")
setDT(cdc_prod_filiere)
cdc_prod_filiere
cdc_prod_filiere <- cdc_prod_filiere[, date_heure := as.POSIXct(paste(date, heure))]
cdc_prod_filiere <- cdc_prod_filiere[, list(date_heure, prod_total, prod_gaz, prod_bioenergies, 
                                            prod_hydraulique, prod_thermique_fossile, prod_charbon, prod_eolien, 
                                            prod_solaire, prod_nucleaire, prod_fioul)]


cdc_prod_filiere <- as.data.frame(cdc_prod_filiere)

devtools::use_data(cdc_prod_filiere, overwrite = TRUE)

