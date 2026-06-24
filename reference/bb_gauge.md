# Gauge property for a Billboard.js chart

Gauge property for a Billboard.js chart

## Usage

``` r
bb_gauge(bb, ...)
```

## Arguments

- bb:

  A \`billboard\` \`htmlwidget\` object.

- ...:

  See
  \<https://naver.github.io/billboard.js/release/latest/doc/Options.html#.gauge\>

## Value

A \`billboard\` \`htmlwidget\` object.

## Examples

``` r
billboarder() %>%
  bb_gaugechart(value = 50) %>%
  bb_gauge(
    min = 0, max = 200, units = "km/h", width = 10,
    label = list(format = htmlwidgets::JS("function(value) {return value;}"))
  )

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"json":{"Value":[50]},"type":"gauge"},"gauge":{"min":0,"max":200,"units":"km/h","width":10,"label":{"format":"function(value) {return value;}"}},"color":{"pattern":["#FF0000","#F97600","#F6C600","#60B044"],"threshold":{"values":[30,60,90,100]}}},"data":null},"evals":["bb_opts.gauge.label.format"],"jsHooks":[]}
```
