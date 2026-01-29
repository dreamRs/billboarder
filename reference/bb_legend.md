# Add legend parameters

Add legend parameters

## Usage

``` r
bb_legend(bb, ...)
```

## Arguments

- bb:

  A `billboard` `htmlwidget` object.

- ...:

  Arguments defined in
  <https://naver.github.io/billboard.js/release/latest/doc/Options.html#.legend>.

## Value

A `billboard` `htmlwidget` object.

## Examples

``` r
library("billboarder")

stars <- data.frame(
  package = c("billboarder", "ggiraph", "officer", "shinyWidgets", "visNetwork"),
  stars = c(1, 176, 42, 40, 166)
)

# Hide legend
billboarder() %>%
  bb_barchart(data = stars) %>% 
  bb_legend(show = FALSE)

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"bb-x","json":{"bb-x":["billboarder","ggiraph","officer","shinyWidgets","visNetwork"],"stars":[1,176,42,40,166]},"type":"bar"},"bar":[],"axis":{"x":{"type":"category"},"rotated":false},"legend":{"show":false}},"data":null},"evals":[],"jsHooks":[]}
# Right legend
billboarder() %>%
  bb_piechart(data = stars) %>% 
  bb_legend(position = "right")

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"json":{"billboarder":[1],"ggiraph":[176],"officer":[42],"shinyWidgets":[40],"visNetwork":[166]},"type":"pie"},"pie":[],"legend":{"position":"right"}},"data":null},"evals":[],"jsHooks":[]}
# Inset legend
billboarder() %>%
  bb_scatterplot(data = iris, x = "Sepal.Length", y = "Sepal.Width", group = "Species") %>% 
  bb_axis(x = list(tick = list(fit = FALSE))) %>% 
  bb_legend(position = "inset", inset = list(anchor = "top-right"))

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"xs":{"setosa":"setosa_x","versicolor":"versicolor_x","virginica":"virginica_x"},"json":{"setosa_x":[5.1,4.9,4.7,4.6,5,5.4,4.6,5,4.4,4.9,5.4,4.8,4.8,4.3,5.8,5.7,5.4,5.1,5.7,5.1,5.4,5.1,4.6,5.1,4.8,5,5,5.2,5.2,4.7,4.8,5.4,5.2,5.5,4.9,5,5.5,4.9,4.4,5.1,5,4.5,4.4,5,5.1,4.8,5.1,4.6,5.3,5],"versicolor_x":[7,6.4,6.9,5.5,6.5,5.7,6.3,4.9,6.6,5.2,5,5.9,6,6.1,5.6,6.7,5.6,5.8,6.2,5.6,5.9,6.1,6.3,6.1,6.4,6.6,6.8,6.7,6,5.7,5.5,5.5,5.8,6,5.4,6,6.7,6.3,5.6,5.5,5.5,6.1,5.8,5,5.6,5.7,5.7,6.2,5.1,5.7],"virginica_x":[6.3,5.8,7.1,6.3,6.5,7.6,4.9,7.3,6.7,7.2,6.5,6.4,6.8,5.7,5.8,6.4,6.5,7.7,7.7,6,6.9,5.6,7.7,6.3,6.7,7.2,6.2,6.1,6.4,7.2,7.4,7.9,6.4,6.3,6.1,7.7,6.3,6.4,6,6.9,6.7,6.9,5.8,6.8,6.7,6.7,6.3,6.5,6.2,5.9],"setosa":[3.5,3,3.2,3.1,3.6,3.9,3.4,3.4,2.9,3.1,3.7,3.4,3,3,4,4.4,3.9,3.5,3.8,3.8,3.4,3.7,3.6,3.3,3.4,3,3.4,3.5,3.4,3.2,3.1,3.4,4.1,4.2,3.1,3.2,3.5,3.6,3,3.4,3.5,2.3,3.2,3.5,3.8,3,3.8,3.2,3.7,3.3],"versicolor":[3.2,3.2,3.1,2.3,2.8,2.8,3.3,2.4,2.9,2.7,2,3,2.2,2.9,2.9,3.1,3,2.7,2.2,2.5,3.2,2.8,2.5,2.8,2.9,3,2.8,3,2.9,2.6,2.4,2.4,2.7,2.7,3,3.4,3.1,2.3,3,2.5,2.6,3,2.6,2.3,2.7,3,2.9,2.9,2.5,2.8],"virginica":[3.3,2.7,3,2.9,3,3,2.5,2.9,2.5,3.6,3.2,2.7,3,2.5,2.8,3.2,3,3.8,2.6,2.2,3.2,2.8,2.8,2.7,3.3,3.2,2.8,3,2.8,3,2.8,3.8,2.8,2.8,2.6,3,3.4,3.1,3,3.1,3.1,3.1,2.7,3.2,3.3,3,2.5,3,3.4,3]},"type":"scatter"},"legend":{"show":true,"position":"inset","inset":{"anchor":"top-right"}},"axis":{"x":{"label":{"text":"Sepal.Length"},"tick":{"fit":false}},"y":{"label":{"text":"Sepal.Width"}}}},"data":null},"evals":[],"jsHooks":[]}
```
