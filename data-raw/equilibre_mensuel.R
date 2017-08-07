

# Equilibre national mensuel production = consommation brute (janvier 2007 à juin 2017) -----------------


# source : RTE -  https://rte-opendata.opendatasoft.com/explore/dataset/equilibre_mensuel_prod_conso_brute/

# date extraction : 06/08/2017


library("jsonlite")
library("zoo")

equilibre_mensuel <- fromJSON(txt = "https://rte-opendata.opendatasoft.com/api/records/1.0/search/?dataset=equilibre_mensuel_prod_conso_brute&rows=-1&sort=-date")
str(equilibre_mensuel, max.level = 2)

equilibre_mensuel <- equilibre_mensuel$records$fields
equilibre_mensuel$date <- as.Date(as.yearmon(equilibre_mensuel$date))
str(equilibre_mensuel)

devtools::use_data(equilibre_mensuel, overwrite = TRUE)
