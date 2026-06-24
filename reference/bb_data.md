# Add data to Billboard chart

Add data to Billboard chart

## Usage

``` r
bb_data(bb, ...)
```

## Arguments

- bb:

  A \`billboard\` \`htmlwidget\` object.

- ...:

  Arguments defined in \<https://naver.github.io/billboard.js/demo/\>.

## Value

A \`billboard\` \`htmlwidget\` object.

## Note

This function can be used with \[billboarderProxy()\] in Shiny
applications.

## Examples

``` r
billboarder() %>%
  bb_barchart(data = table(mtcars$cyl)) %>%
  bb_data(names = list(Freq = "Number of cylinders"), labels = TRUE)

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"bb-x","json":{"bb-x":["4","6","8"],"Freq":[11,7,14]},"type":"bar","names":{"Freq":"Number of cylinders"},"labels":true},"bar":[],"axis":{"x":{"type":"category"},"rotated":false}},"data":null},"evals":[],"jsHooks":[]}
```
