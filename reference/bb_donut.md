# Donut property for a Billboard.js chart

Donut property for a Billboard.js chart

## Usage

``` r
bb_donut(bb, ...)
```

## Arguments

- bb:

  A `billboard` `htmlwidget` object.

- ...:

  See
  <https://naver.github.io/billboard.js/release/latest/doc/Options.html#.donut>

## Value

A `billboard` `htmlwidget` object.

## Examples

``` r
billboarder() %>%
  bb_donutchart(data = table(mtcars$cyl)) %>%
  bb_donut(title = "Donut Title", width = 10)

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"json":{"4":[11],"6":[7],"8":[14]},"type":"donut"},"donut":{"title":"Donut Title","width":10}},"data":null},"evals":[],"jsHooks":[]}  
```
