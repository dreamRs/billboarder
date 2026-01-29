# Pie property for a Billboard.js chart

Pie property for a Billboard.js chart

## Usage

``` r
bb_pie(bb, ...)
```

## Arguments

- bb:

  A `billboard` `htmlwidget` object.

- ...:

  See
  <https://naver.github.io/billboard.js/release/latest/doc/Options.html#.pie>

## Value

A `billboard` `htmlwidget` object.

## Examples

``` r
billboarder() %>%
  bb_piechart(data = table(mtcars$cyl)) %>% 
  bb_pie(label = list(
    ratio = 0.5, 
    format = htmlwidgets::JS("function(value) {return d3.format('$')(value);}")
  ), 
  expand = FALSE)

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"json":{"4":[11],"6":[7],"8":[14]},"type":"pie"},"pie":{"label":{"ratio":0.5,"format":"function(value) {return d3.format('$')(value);}"},"expand":false}},"data":null},"evals":["bb_opts.pie.label.format"],"jsHooks":[]}  
```
