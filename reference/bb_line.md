# Line property for a Billboard.js chart

Line property for a Billboard.js chart

## Usage

``` r
bb_line(bb, ...)
```

## Arguments

- bb:

  A \`billboard\` \`htmlwidget\` object.

- ...:

  See
  \<https://naver.github.io/billboard.js/release/latest/doc/Options.html#.line\>

## Value

A \`billboard\` \`htmlwidget\` object.

## Examples

``` r
# Set if null data point will be connected or not.
b <- billboarder() %>%
  bb_linechart(data = c(1, 2, NA, 4, 5))
b

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"line":{"classes":["billboarder-line-x"]},"data":{"x":"index","json":{"index":[1,2,3,4,5],"x":[1,2,null,4,5]},"types":{"x":"line"}},"point":{"show":false}},"data":null},"evals":[],"jsHooks":[]}b %>% bb_line(connectNull = TRUE)

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"line":{"classes":["billboarder-line-x"],"connectNull":true},"data":{"x":"index","json":{"index":[1,2,3,4,5],"x":[1,2,null,4,5]},"types":{"x":"line"}},"point":{"show":false}},"data":null},"evals":[],"jsHooks":[]}
```
