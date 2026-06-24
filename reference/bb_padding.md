# The padding of the chart element.

The padding of the chart element.

## Usage

``` r
bb_padding(bb, ...)
```

## Arguments

- bb:

  A \[billboarder()\] \`htmlwidget\` object or a \[billboarderProxy()\]
  \`htmlwidget\` object.

- ...:

  See
  \<https://naver.github.io/billboard.js/release/latest/doc/Options.html#.padding\>
  for possible options.

## Value

A \`billboard\` \`htmlwidget\` object.

## Examples

``` r
# Add padding around the chart
billboarder() %>%
  bb_barchart(data = table(sample(letters[1:5], 100, TRUE))) %>%
  bb_padding(top = 100, right = 100, bottom = 100, left = 100)

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"bb-x","json":{"bb-x":["a","b","c","d","e"],"Freq":[18,16,23,26,17]},"type":"bar"},"bar":[],"axis":{"x":{"type":"category"},"rotated":false},"padding":{"top":100,"right":100,"bottom":100,"left":100}},"data":null},"evals":[],"jsHooks":[]}
```
