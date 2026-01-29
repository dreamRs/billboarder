# French electricity generation by power source for the day of 2017-06-12.

Average power generation (MW) per 30-minute interval within continental
France, aggregated by broad power source. Last update : 2017-07-27.

## Usage

``` r
cdc_prod_filiere
```

## Format

A data frame with 48 rows and 11 variables:

- date_heure:

  Timestamp (POSIXct)

- prod_total:

  Total production in MW (thermal + hydro + nuclear + solar + wind +
  bioenergy)

- prod_gaz:

  Gas production in MW

- prod_bioenergies:

  Bioenergy production in MW

- prod_hydraulique:

  Hydraulic production in MW

- prod_thermique_fossile:

  Fossil thermal production in MW

- prod_charbon:

  Coal production in MW

- prod_eolien:

  Wind production in MW

- prod_solaire:

  Solar production in MW

- prod_nucleaire:

  Nuclear production in MW

- prod_fioul:

  Oil production in MW

## Source

RTE
