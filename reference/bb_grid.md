# Grid property for a Billboard.js chart

Grid property for a Billboard.js chart

## Usage

``` r
bb_grid(bb, ...)

bb_x_grid(bb, ...)

bb_y_grid(bb, ...)
```

## Arguments

- bb:

  A \`billboard\` \`htmlwidget\` object.

- ...:

  See
  \<https://naver.github.io/billboard.js/release/latest/doc/Options.html#.grid\>

## Value

A \`billboard\` \`htmlwidget\` object.

## Note

\[bb_x_grid()\] and \[bb_y_grid()\] are shortcuts for modifying the
x-axis and the y-axis respectively.

## Examples

``` r
stars <- data.frame(
  package = c("billboarder", "ggiraph", "officer", "shinyWidgets", "visNetwork"),
  stars = c(1, 176, 42, 40, 166)
)

billboarder() %>%
  bb_barchart(data = stars) %>%
  bb_y_grid(show = TRUE)

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"bb-x","json":{"bb-x":["billboarder","ggiraph","officer","shinyWidgets","visNetwork"],"stars":[1,176,42,40,166]},"type":"bar"},"bar":[],"axis":{"x":{"type":"category"},"rotated":false},"grid":{"y":{"show":true}}},"data":null},"evals":[],"jsHooks":[]}
billboarder() %>%
  bb_barchart(data = stars) %>%
  bb_y_grid(lines = list(list(value = mean(stars$stars), text = "Horizontal line")))

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"bb-x","json":{"bb-x":["billboarder","ggiraph","officer","shinyWidgets","visNetwork"],"stars":[1,176,42,40,166]},"type":"bar"},"bar":[],"axis":{"x":{"type":"category"},"rotated":false},"grid":{"y":{"lines":[{"value":85,"text":"Horizontal line"}]}}},"data":null},"evals":[],"jsHooks":[]}
```
