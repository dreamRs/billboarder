# Bar property for a Billboard.js chart

Bar property for a Billboard.js chart

## Usage

``` r
bb_bar(bb, ...)
```

## Arguments

- bb:

  A `billboard` `htmlwidget` object.

- ...:

  See
  <https://naver.github.io/billboard.js/release/latest/doc/Options.html#.bar>

## Value

A `billboard` `htmlwidget` object.

## Examples

``` r
billboarder() %>%
  bb_barchart(data = data.frame(v1 = c("a", "b", "c"), value = c(5, 6, 3))) %>% 
  bb_bar(width = list(ratio = 0.95))

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"bb-x","json":{"bb-x":["a","b","c"],"value":[5,6,3]},"type":"bar"},"bar":{"width":{"ratio":0.95}},"axis":{"x":{"type":"category"},"rotated":false}},"data":null},"evals":[],"jsHooks":[]}  
```
