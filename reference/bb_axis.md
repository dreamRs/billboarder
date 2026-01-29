# Add axis parameters

Add axis parameters

## Usage

``` r
bb_axis(bb, ...)

bb_x_axis(bb, ...)

bb_y_axis(bb, ...)
```

## Arguments

- bb:

  A `billboard` `htmlwidget` object.

- ...:

  Arguments defined in <https://naver.github.io/billboard.js/demo/>.

## Value

A `billboard` `htmlwidget` object.

## Examples

``` r
stars <- data.frame(
  package = c("billboarder", "ggiraph", "officer", "shinyWidgets", "visNetwork"),
  stars = c(9, 178, 43, 46, 175)
)

# Add a label to y axis
billboarder() %>% 
  bb_barchart(data = stars) %>% 
  bb_axis(y = list(label = list(text = "# of stars", position = "middle")))

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"bb-x","json":{"bb-x":["billboarder","ggiraph","officer","shinyWidgets","visNetwork"],"stars":[9,178,43,46,175]},"type":"bar"},"bar":[],"axis":{"x":{"type":"category"},"rotated":false,"y":{"label":{"text":"# of stars","position":"middle"}}}},"data":null},"evals":[],"jsHooks":[]}  
# or shorter :
billboarder() %>% 
  bb_barchart(data = stars) %>% 
  bb_y_axis(label = list(text = "# of stars", position = "outer-top"))

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"bb-x","json":{"bb-x":["billboarder","ggiraph","officer","shinyWidgets","visNetwork"],"stars":[9,178,43,46,175]},"type":"bar"},"bar":[],"axis":{"x":{"type":"category"},"rotated":false,"y":{"label":{"text":"# of stars","position":"outer-top"}}}},"data":null},"evals":[],"jsHooks":[]}
```
