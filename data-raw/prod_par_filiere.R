

# Production nationale annuelle par filiere (2012 a 2016) -----------------


# source : RTE -  https://opendata.rte-france.com/explore/dataset/prod_par_filiere

# date extraction : 02/08/2017


library("jsonlite")

prod_par_filiere <- fromJSON(txt = "https://opendata.rte-france.com/api/records/1.0/search/?dataset=prod_par_filiere&rows=-1&sort=-annee&facet=annee")
prod_par_filiere <- prod_par_filiere$records$fields
prod_par_filiere <- prod_par_filiere[, c(11, 2, 1, 3:10)]

devtools::use_data(prod_par_filiere, overwrite = TRUE)


# Format long
prod_filiere_long <- reshape(
  data = prod_par_filiere, 
  idvar = "annee",
  varying = list(2:11), 
  times = names(prod_par_filiere)[-1],
  timevar = "branche",
  direction = "long"
)
rownames(prod_filiere_long) <- NULL
prod_filiere_long$branche <- gsub("prod_", "", prod_filiere_long$branche)
names(prod_filiere_long)[3] <- "prod"
prod_filiere_long <- as.data.frame(prod_filiere_long)
attr(prod_filiere_long, "reshapeLong") <- NULL

prod_filiere_long <- prod_filiere_long[prod_filiere_long$branche != "total", ]

devtools::use_data(prod_filiere_long, overwrite = TRUE)




