# Helper for creating a donut chart

Helper for creating a donut chart

## Usage

``` r
bb_donutchart(bb, data, mapping = NULL, ...)
```

## Arguments

- bb:

  A `billboard` `htmlwidget` object.

- data:

  A `data.frame`.

- mapping:

  Mapping of variables on the chart, see
  [`bbaes`](https://dreamrs.github.io/billboarder/reference/billboard-aes.md).

- ...:

  Arguments for slot donut,
  <https://naver.github.io/billboard.js/release/latest/doc/Options.html#.donut>.

## Value

A `billboard` `htmlwidget` object.

## Note

This function can be used with
[`billboarderProxy`](https://dreamrs.github.io/billboarder/reference/billboarder-shiny.md)
in shiny application.

## Examples

``` r
if (FALSE) { # \dontrun{
stars <- data.frame(
  package = c("billboarder", "ggiraph", "officer", "shinyWidgets", "visNetwork"),
  stars = c(9, 177, 43, 44, 169)
)

billboarder() %>% 
  bb_donutchart(data = stars, title = "Stars")
} # }
```
