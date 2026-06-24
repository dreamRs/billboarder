# Area property for a Billboard.js chart

Area property for a Billboard.js chart

## Usage

``` r
bb_area(bb, ...)
```

## Arguments

- bb:

  A \`billboard\` \`htmlwidget\` object.

- ...:

  See
  \<https://naver.github.io/billboard.js/release/latest/doc/Options.html#.area\>

## Value

A \`billboard\` \`htmlwidget\` object.

## Examples

``` r
# Area chart options
billboarder() %>%
  bb_linechart(data = c(2, 4, 3, 6, 5), type = "area") %>%
  bb_area(linearGradient = TRUE)

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"line":{"classes":["billboarder-line-x"]},"data":{"x":"index","json":{"index":[1,2,3,4,5],"x":[2,4,3,6,5]},"types":{"x":"area"}},"point":{"show":false},"area":{"linearGradient":true}},"data":null},"evals":[],"jsHooks":[]}
```
