# SVG property for a Billboard.js chart

SVG property for a Billboard.js chart

## Usage

``` r
bb_svg(bb, ...)
```

## Arguments

- bb:

  A \`billboard\` \`htmlwidget\` object.

- ...:

  See
  \<https://naver.github.io/billboard.js/release/latest/doc/Options.html#.svg\>

## Value

A \`billboard\` \`htmlwidget\` object.

## Examples

``` r
# Set SVG class name
billboarder() %>%
  bb_linechart(data = c(2, 5, 3, 4, 6)) %>%
  bb_svg(classname = "custom-billboard")

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"line":{"classes":["billboarder-line-x"]},"data":{"x":"index","json":{"index":[1,2,3,4,5],"x":[2,5,3,4,6]},"types":{"x":"line"}},"point":{"show":false},"svg":{"classname":"custom-billboard"}},"data":null},"evals":[],"jsHooks":[]}
```
