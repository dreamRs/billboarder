# Treemap property for a Billboard.js chart

Treemap property for a Billboard.js chart

## Usage

``` r
bb_treemap(bb, ...)
```

## Arguments

- bb:

  A `billboard` `htmlwidget` object.

- ...:

  See
  <https://naver.github.io/billboard.js/release/latest/doc/Options.html#.treemap>

## Value

A `billboard` `htmlwidget` object.

## Examples

``` r
library("billboarder")
data("mpg", package = "ggplot2")

billboarder() %>% 
  bb_treemapchart(mpg[, 1]) %>% 
  bb_treemap(label = list(show = TRUE, threshold = 0.03))%>% 
  bb_data(
    labels = list(colors = "#FFF")
  )

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"json":{"audi":[18],"chevrolet":[19],"dodge":[37],"ford":[25],"honda":[9],"hyundai":[14],"jeep":[8],"land rover":[4],"lincoln":[3],"mercury":[4],"nissan":[13],"pontiac":[5],"subaru":[14],"toyota":[34],"volkswagen":[27]},"type":"treemap","labels":{"colors":"#FFF"}},"treemap":{"label":{"show":true,"threshold":0.03}}},"data":null},"evals":[],"jsHooks":[]}
```
