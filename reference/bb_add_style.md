# Add custom style for regions and grid lines

Add custom style for regions and grid lines

## Usage

``` r
bb_add_style(
  bb,
  region = NULL,
  x_grid = NULL,
  y_grid = NULL,
  ...,
  .list = NULL
)
```

## Arguments

- bb:

  A `billboard` `htmlwidget` object.

- region:

  A named list with style associated with region.

- x_grid:

  A named list with style associated with grid line on the X-axis.

- y_grid:

  A named list with style associated with grid line on the Y-axis.

- ..., .list:

  Used internally.

## Value

A `billboard` `htmlwidget` object.

## Examples

``` r
# Change default color for regions
billboarder() %>% 
  bb_linechart(data = sin(seq(-pi, pi, length.out = 30))) %>% 
  bb_regions(
    list(start = 0, end = 10, class = "custom"), # add custom class
    list(start = 19, end = 29, class = "foo")
  ) %>% 
  bb_add_style(region = list(custom = "fill: red;", foo = "fill: #009246;"))

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"line":{"classes":["billboarder-line-x"]},"data":{"x":"index","json":{"index":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30],"x":[-1.224646799147353e-16,-0.2149704402110243,-0.4198891015602648,-0.6051742151937655,-0.7621620551276365,-0.8835120444460229,-0.963549992519223,-0.9985334138511238,-0.9868265225415261,-0.9289767198167914,-0.8276889981568906,-0.6876994588534232,-0.5155538571770215,-0.3193015301359797,-0.1081190184239418,0.1081190184239418,0.3193015301359801,0.515553857177022,0.6876994588534235,0.8276889981568906,0.9289767198167914,0.9868265225415261,0.9985334138511238,0.9635499925192229,0.8835120444460227,0.7621620551276361,0.6051742151937648,0.4198891015602648,0.2149704402110243,1.224646799147353e-16]},"types":{"x":"line"}},"point":{"show":false},"regions":[{"start":0,"end":10,"class":"custom"},{"start":19,"end":29,"class":"foo"}],"customStyle":[".bb-region.custom{fill: red;}.bb-region.foo{fill: #009246;}"]},"data":null},"evals":[],"jsHooks":[]}
# Customize grid line and text
billboarder() %>% 
  bb_linechart(data = sin(seq(-pi, pi, length.out = 30))) %>% 
  bb_y_grid(lines = list(list(
    value = 0, text = "Zero", position  = "middle", class = "zero"
  ))) %>% 
  bb_add_style(y_grid = list(
    zero = list(line = "stroke: red", text = "font-size: 240%; fill: black"
  )))

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"line":{"classes":["billboarder-line-x"]},"data":{"x":"index","json":{"index":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30],"x":[-1.224646799147353e-16,-0.2149704402110243,-0.4198891015602648,-0.6051742151937655,-0.7621620551276365,-0.8835120444460229,-0.963549992519223,-0.9985334138511238,-0.9868265225415261,-0.9289767198167914,-0.8276889981568906,-0.6876994588534232,-0.5155538571770215,-0.3193015301359797,-0.1081190184239418,0.1081190184239418,0.3193015301359801,0.515553857177022,0.6876994588534235,0.8276889981568906,0.9289767198167914,0.9868265225415261,0.9985334138511238,0.9635499925192229,0.8835120444460227,0.7621620551276361,0.6051742151937648,0.4198891015602648,0.2149704402110243,1.224646799147353e-16]},"types":{"x":"line"}},"point":{"show":false},"grid":{"y":{"lines":[{"value":0,"text":"Zero","position":"middle","class":"zero"}]}},"customStyle":[".bb-ygrid-line.zero line{stroke: red} .bb-ygrid-line.zero text{font-size: 240%; fill: black}"]},"data":null},"evals":[],"jsHooks":[]}
```
