# Render property for a Billboard.js chart

Render property for a Billboard.js chart

## Usage

``` r
bb_render(bb, ...)
```

## Arguments

- bb:

  A \[billboarder()\] \`htmlwidget\` object or a \[billboarderProxy()\]
  \`htmlwidget\` object.

- ...:

  See
  \<https://naver.github.io/billboard.js/release/latest/doc/Options.html#.render\>
  for possible options.

## Value

A \`billboard\` \`htmlwidget\` object.

## Examples

``` r
# Customize render options
billboarder() %>%
  bb_linechart(data = c(10, 20, 15, 30)) %>%
  bb_render(mode = "canvas")

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"line":{"classes":["billboarder-line-x"]},"data":{"x":"index","json":{"index":[1,2,3,4],"x":[10,20,15,30]},"types":{"x":"line"}},"point":{"show":false},"render":{"mode":"canvas"}},"data":null},"evals":[],"jsHooks":[]}
```
