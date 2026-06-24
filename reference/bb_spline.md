# Spline property for a Billboard.js chart

Spline property for a Billboard.js chart

## Usage

``` r
bb_spline(bb, ...)
```

## Arguments

- bb:

  A \`billboard\` \`htmlwidget\` object.

- ...:

  See
  \<https://naver.github.io/billboard.js/release/latest/doc/Options.html#.spline\>

## Value

A \`billboard\` \`htmlwidget\` object.

## Examples

``` r
# Customize spline interpolation
billboarder() %>%
  bb_linechart(data = c(1, 5, 3, 6, 2), type = "spline") %>%
  bb_spline(interpolation = list(type = "natural"))

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"line":{"classes":["billboarder-line-x"]},"data":{"x":"index","json":{"index":[1,2,3,4,5],"x":[1,5,3,6,2]},"types":{"x":"spline"}},"point":{"show":false},"spline":{"interpolation":{"type":"natural"}}},"data":null},"evals":[],"jsHooks":[]}
```
