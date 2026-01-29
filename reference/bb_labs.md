# Quickly set title, axis labels and caption

Quickly set title, axis labels and caption

## Usage

``` r
bb_labs(bb, title = NULL, x = NULL, y = NULL, caption = NULL, ...)
```

## Arguments

- bb:

  A `billboard` `htmlwidget` object.

- title:

  Text for the chart title, use `\n` to make a new line.

- x:

  Text for x axis title.

- y:

  Text for y axis title.

- caption:

  Text for the caption displayed in the bottom-right of the chart.

- ...:

  Not used.

## Value

A `billboard` `htmlwidget` object.

## Note

`caption` is not part of the billboard.js library, it is added by the
`billboarder` package.

## Examples

``` r
data("prod_par_filiere")

billboarder() %>%
  bb_barchart(
    data = prod_par_filiere[, c("annee", "prod_hydraulique")],
    color = "#102246"
  ) %>%
  bb_legend(show = FALSE) %>%
  bb_labs(
    title = "French hydraulic production",
    y = "production (in terawatt-hours)",
    caption = "Data source: RTE (https://opendata.reseaux-energies.fr/)",
    caption_href = "https://opendata.reseaux-energies.fr/"
  )

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"bb-x","json":{"bb-x":["2012","2013","2014","2015","2016"],"prod_hydraulique":[63.8,75.5,68.09999999999999,59.1,63.9]},"type":"bar"},"bar":[],"color":{"pattern":["#102246"]},"axis":{"x":{"type":"category"},"rotated":false,"y":{"label":{"text":"production (in terawatt-hours)","position":"outer-top"}}},"legend":{"show":false},"customStyle":".bb-title tspan:nth-child(2){font-size: smaller; font-weight: lighter;}","padding":{"top":30},"title":{"text":"French hydraulic production\nData source: RTE (https://opendata.reseaux-energies.fr/)","position":"left-top"}},"data":null},"evals":[],"jsHooks":[]}
```
