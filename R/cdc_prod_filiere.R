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