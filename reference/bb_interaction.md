# Interaction property for a Billboard.js chart

Interaction property for a Billboard.js chart

## Usage

``` r
bb_interaction(bb, ...)
```

## Arguments

- bb:

  A \`billboard\` \`htmlwidget\` object.

- ...:

  See
  \<https://naver.github.io/billboard.js/release/latest/doc/Options.html#.interaction\>

## Value

A \`billboard\` \`htmlwidget\` object.

## Examples

``` r
# Disable interactions
billboarder() %>%
  bb_linechart(data = c(1, 3, 2, 5, 4)) %>%
  bb_interaction(enabled = FALSE)

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false},"enabled":false},"line":{"classes":["billboarder-line-x"]},"data":{"x":"index","json":{"index":[1,2,3,4,5],"x":[1,3,2,5,4]},"types":{"x":"line"}},"point":{"show":false}},"data":null},"evals":[],"jsHooks":[]}
# Only mouse input
billboarder() %>%
  bb_linechart(data = c(1, 3, 2, 5, 4)) %>%
  bb_interaction(inputType = list(touch = FALSE))

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"line":{"classes":["billboarder-line-x"]},"data":{"x":"index","json":{"index":[1,2,3,4,5],"x":[1,3,2,5,4]},"types":{"x":"line"}},"point":{"show":false}},"data":null},"evals":[],"jsHooks":[]}
```
