#' Monthly supply / demand balance (january 2007 to june 2017)
#'
#' Monthly history of supply/demand balance (GWh) based on gross consumption, 
#' the balance of physical exchanges with foreign countries and offtakes due to pumping.
#' Last update : 2017-07-27.
#'
#' @format A data frame with 126 rows and 5 variables:
#' \describe{
#'   \item{date}{Date}
#'   \item{solde}{Supply/demand balance (in GWh)}
#'   \item{production}{Generation (in GWh)}
#'   \item{pompage}{Pumping for hydraulic generation (in GWh)}
#'   \item{consommation}{Consumption (in GWh)}
#' }
#' @source RTE (\url{https://opendata.reseaux-energies.fr/explore/dataset/equilibre-national-mensuel-prod-conso-brute/})
"equilibre_mensuel"



#' French electricity generation by year and branch.
#'
#' Annual French electricity production (TWh) by branch. Last update : 2017-02-15.
#'
#' @format A data frame with 5 rows and 11 variables:
#' \describe{
#'   \item{annee}{Year}
#'   \item{prod_total}{Total production in TWh (thermal + hydro + nuclear + solar + wind + bioenergy)}
#'   \item{prod_therm}{Thermal production in TWh (oil + gas + coal)}
#'   \item{prod_hydraulique}{Hydraulic production in TWh}
#'   \item{prod_bioenergies}{Bioenergy production in TWh}
#'   \item{prod_eolien}{Wind production in TWh}
#'   \item{prod_therm_charbon}{Coal thermal production in TWh}
#'   \item{prod_solaire}{Solar production in TWh}
#'   \item{prod_therm_gaz}{Gaz thermal production in TWh}
#'   \item{prod_nucleaire}{Nuclear production in TWh}
#'   \item{prod_therm_fioul}{Oil thermal production in TWh}
#' }
#' @source RTE (\url{https://opendata.rte-france.com/explore/dataset/prod_par_filiere})
"prod_par_filiere"


#' French electricity generation by year and branch.
#'
#' Annual French electricity production (TWh) by branch. Last update : 2017-02-15.
#'
#' @format A data frame with 45 rows and 3 variables:
#' \describe{
#'   \item{annee}{Year}
#'   \item{branche}{Source of production}
#'   \item{prod}{Production in TWh}
#' }
#' @source RTE (\url{https://opendata.rte-france.com/explore/dataset/prod_par_filiere})
"prod_filiere_long"




#' French electricity generation by power source for the day of 2017-06-12.
#'
#' Average power generation (MW) per 30-minute interval within continental France, aggregated by broad power source. Last update : 2017-07-27.
#'
#' @format A data frame with 48 rows and 11 variables:
#' \describe{
#'   \item{date_heure}{Timestamp (POSIXct)}
#'   \item{prod_total}{Total production in MW (thermal + hydro + nuclear + solar + wind + bioenergy)}
#'   \item{prod_gaz}{Gas production in MW}
#'   \item{prod_bioenergies}{Bioenergy production in MW}
#'   \item{prod_hydraulique}{Hydraulic production in MW}
#'   \item{prod_thermique_fossile}{Fossil thermal production in MW}
#'   \item{prod_charbon}{Coal production in MW}
#'   \item{prod_eolien}{Wind production in MW}  
#'   \item{prod_solaire}{Solar production in MW}   
#'   \item{prod_nucleaire}{Nuclear production in MW}
#'   \item{prod_fioul}{Oil production in MW}
#' }
#' @source RTE (\url{https://opendata.reseaux-energies.fr/explore/dataset/production-quotidienne-filiere})
"cdc_prod_filiere"




#' Power ratings for The Avengers.
#' 
#' Data are available in "long" and "wide" format.
#'
#' @format A data frame with 24 rows and 4 variables:
#' \describe{
#'   \item{group}{Name of the hero}
#'   \item{axis}{Power skill}
#'   \item{value}{Value (1-7)}
#'   \item{description}{Character description}
#' }
#' @source Marvel Wikia (\url{https://marvel.fandom.com/wiki/Marvel_Database})
#'  and Chris Zhou (\url{http://bl.ocks.org/chrisrzhou/2421ac6541b68c1680f8})
"avengers"

#' @rdname avengers
"avengers_wide"



