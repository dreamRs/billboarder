# Set theme and default colors for Billboard charts

Set theme and default colors for Billboard charts

## Usage

``` r
set_theme(name = c("billboard", "insight", "graph", "datalab", "modern"))

set_color_palette(colors)
```

## Arguments

- name:

  Name of the theme, possible values are : \`"billboard"\`,
  \`"insight"\`, \`"graph"\`, \`"datalab"\`, \`"modern"\`.

- colors:

  Vector of colors to use as default.

## Note

You can only use one theme and palette at a time (in Shiny applications
or Markdown documents).

## Examples

``` r
library("billboarder")
set_theme("insight")

data("prod_par_filiere")
billboarder() %>%
  bb_barchart(
    data = prod_par_filiere[, c("annee", "prod_hydraulique", "prod_eolien", "prod_solaire")]
  ) %>%
  bb_data(
    names = list(prod_hydraulique = "Hydraulic", prod_eolien = "Wind", prod_solaire = "Solar")
  ) %>% 
  bb_y_grid(show = TRUE) %>%
  bb_y_axis(tick = list(format = suffix("TWh")),
            label = list(text = "production (in terawatt-hours)", position = "outer-top")) %>% 
  bb_legend(position = "inset", inset = list(anchor = "top-right")) %>% 
  bb_labs(title = "Renewable energy production",
          caption = "Data source: RTE (https://opendata.rte-france.com)")

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"bb-x","json":{"bb-x":["2012","2013","2014","2015","2016"],"prod_hydraulique":[63.8,75.5,68.09999999999999,59.1,63.9],"prod_eolien":[14.9,15.9,17.1,21.1,20.7],"prod_solaire":[4.1,4.7,5.9,7.4,8.300000000000001]},"type":"bar","names":{"prod_hydraulique":"Hydraulic","prod_eolien":"Wind","prod_solaire":"Solar"}},"bar":[],"axis":{"x":{"type":"category"},"rotated":false,"y":{"tick":{"format":"function(x) {return x + 'TWh';}"},"label":{"text":"production (in terawatt-hours)","position":"outer-top"}}},"grid":{"y":{"show":true}},"legend":{"position":"inset","inset":{"anchor":"top-right"}},"customStyle":".bb-title tspan:nth-child(2){font-size: smaller; font-weight: lighter;}","padding":{"top":30},"title":{"text":"Renewable energy production\nData source: RTE (https://opendata.rte-france.com)","position":"left-top"}},"data":null},"evals":["bb_opts.axis.y.tick.format"],"jsHooks":[]}
```
