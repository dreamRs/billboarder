# Transition property for a Billboard.js chart

Transition property for a Billboard.js chart

## Usage

``` r
bb_transition(bb, ...)
```

## Arguments

- bb:

  A \`billboard\` \`htmlwidget\` object.

- ...:

  See
  \<https://naver.github.io/billboard.js/release/latest/doc/Options.html#.transition\>

## Value

A \`billboard\` \`htmlwidget\` object.

## Examples

``` r
# Set transition duration
billboarder() %>%
  bb_linechart(data = c(5, 3, 6, 2, 7)) %>%
  bb_transition(duration = 1000)

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"line":{"classes":["billboarder-line-x"]},"data":{"x":"index","json":{"index":[1,2,3,4,5],"x":[5,3,6,2,7]},"types":{"x":"line"}},"point":{"show":false},"transition":{"duration":1000}},"data":null},"evals":[],"jsHooks":[]}
```
