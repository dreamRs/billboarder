# Helper for creating a pie chart

Helper for creating a pie chart

## Usage

``` r
bb_piechart(bb, data, mapping = NULL, ...)
```

## Arguments

- bb:

  A \`billboard\` \`htmlwidget\` object.

- data:

  A \`data.frame\`, first column should contain labels, second column
  values associated, except if mapping is provided.

- mapping:

  Mapping of variables on the chart, see \[bbaes()\].

- ...:

  Arguments for slot pie,
  \<https://naver.github.io/billboard.js/release/latest/doc/Options.html#.pie\>.

## Value

A \`billboard\` \`htmlwidget\` object.

## Note

This function can be used with \[billboarderProxy()\] in Shiny
applications.

## Examples

``` r
stars <- data.frame(
  package = c("billboarder", "ggiraph", "officer", "shinyWidgets", "visNetwork"),
  stars = c(9, 177, 43, 44, 169)
)

# Default
billboarder() %>%
  bb_piechart(data = stars)

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"json":{"billboarder":[9],"ggiraph":[177],"officer":[43],"shinyWidgets":[44],"visNetwork":[169]},"type":"pie"},"pie":[]},"data":null},"evals":[],"jsHooks":[]}
# Explicit mapping
billboarder() %>%
  bb_piechart(data = stars, bbaes(package, stars))

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"json":{"billboarder":[9],"ggiraph":[177],"officer":[43],"shinyWidgets":[44],"visNetwork":[169]},"type":"pie"},"pie":[]},"data":null},"evals":[],"jsHooks":[]}
# Other way to specify mapping
billboarder(data = stars) %>%
  bb_aes(package, stars) %>%
  bb_piechart()

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"json":{"billboarder":[9],"ggiraph":[177],"officer":[43],"shinyWidgets":[44],"visNetwork":[169]},"type":"pie"},"pie":[]},"data":{"package":["billboarder","ggiraph","officer","shinyWidgets","visNetwork"],"stars":[9,177,43,44,169]},"mapping":{"x":{},"y":{}}},"evals":[],"jsHooks":[]}
```
