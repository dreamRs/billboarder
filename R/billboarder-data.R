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
#' @source RTE (\url{https://opendata.rte-france.com/explore/dataset/equilibre_mensuel_prod_conso_brute})
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



#' French electricity generation by power source for the day of 2017-06-12.
#'
#' Average power generation (MW) per 30-minute interval within continental France, aggregated by broad power source. Last update : 2017-07-27.
#'
#' @format A data frame with 48 rows and 11 variables:
#' \describe{
#'   \item{date_heure}{Timestamp (POSIXct)}
#'   \item{prod_total}{Total production in TWh (thermal + hydro + nuclear + solar + wind + bioenergy)}
#'   \item{prod_gaz}{Gas production in TWh}
#'   \item{prod_bioenergies}{Bioenergy production in TWh}
#'   \item{prod_hydraulique}{Hydraulic production in TWh}
#'   \item{prod_thermique_fossile}{Fossil thermal production in TWh}
#'   \item{prod_charbon}{Coal production in TWh}
#'   \item{prod_eolien}{Wind production in TWh}#'   
#'   \item{prod_solaire}{Solar production in TWh}   
#'   \item{prod_nucleaire}{Nuclear production in TWh}
#'   \item{prod_fioul}{Oil production in TWh}
#' }
#' @source RTE (\url{https://opendata.rte-france.com/explore/dataset/cdc_prod_par_filiere})
"cdc_prod_filiere"
