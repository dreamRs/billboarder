


# Production nationale annuelle par filiere (2012 a 2016) -----------------


# source RTE -  https://opendata.rte-france.com/explore/dataset/prod_par_filiere/information/?sort=-annee

# date extraction : 02/08/2017

library("jsonlite")

prod_par_filiere <- fromJSON(txt = "https://opendata.rte-france.com/api/records/1.0/search/?dataset=prod_par_filiere&rows=-1&sort=-annee&facet=annee")
prod_par_filiere <- prod_par_filiere$records$fields
prod_par_filiere <- prod_par_filiere[, c(11, 2, 1, 3:10)]

devtools::use_data(prod_par_filiere, overwrite = TRUE)


