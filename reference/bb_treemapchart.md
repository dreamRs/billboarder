# Helper for creating a treemap chart

Helper for creating a treemap chart

## Usage

``` r
bb_treemapchart(bb, data, mapping = NULL, ...)
```

## Arguments

- bb:

  A `billboard` `htmlwidget` object.

- data:

  A `data.frame`, the first column will be used for x axis unless
  specified otherwise in `mapping`. If not a `data.frame`, an object
  coercible to `data.frame`.

- mapping:

  Mapping of variables on the chart, see
  [`bbaes`](https://dreamrs.github.io/billboarder/reference/billboard-aes.md).

- ...:

  Arguments passed to
  [`bb_treemap`](https://dreamrs.github.io/billboarder/reference/bb_treemap.md).

## Value

A `billboard` `htmlwidget` object.

## Examples

``` r
library("billboarder")
data("mpg", package = "ggplot2")

billboarder() %>% 
  bb_treemapchart(mpg[, 1])

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"json":{"audi":[18],"chevrolet":[19],"dodge":[37],"ford":[25],"honda":[9],"hyundai":[14],"jeep":[8],"land rover":[4],"lincoln":[3],"mercury":[4],"nissan":[13],"pontiac":[5],"subaru":[14],"toyota":[34],"volkswagen":[27]},"type":"treemap"},"treemap":[]},"data":null},"evals":[],"jsHooks":[]}
billboarder() %>% 
  bb_treemapchart(
    data = mpg, 
    mapping = aes(x = manufacturer),
    label = list(show = TRUE, threshold = 0.3)
  ) %>% 
  bb_data(
    labels = list(colors = "#FFF")
  )
#> Non unique values in 'manufacturer' : calculating sum of 'rep(1, length(manufacturer))'

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"json":{"audi":[18],"chevrolet":[19],"dodge":[37],"ford":[25],"honda":[9],"hyundai":[14],"jeep":[8],"land rover":[4],"lincoln":[3],"mercury":[4],"nissan":[13],"pontiac":[5],"subaru":[14],"toyota":[34],"volkswagen":[27]},"type":"treemap","labels":{"colors":"#FFF"}},"treemap":{"label":{"show":true,"threshold":0.3}}},"data":null},"evals":[],"jsHooks":[]}
```
