# Helper for creating a gauge

Helper for creating a gauge

## Usage

``` r
bb_gaugechart(
  bb,
  value,
  name = "Value",
  color = NULL,
  steps = c(30, 60, 90, 100),
  steps_color = c("#FF0000", "#F97600", "#F6C600", "#60B044"),
  ...
)
```

## Arguments

- bb:

  A \`billboard\` \`htmlwidget\` object.

- value:

  A single numeric value or a vector for stacked gauge.

- name:

  Name for the value, appear in tooltip, same length as \`value\`.

- color:

  Color for the gauge, if provided, \`steps\` and \`steps_color\` are
  ignored.

- steps:

  Upper bound for changing colors

- steps_color:

  Colors corresponding to steps

- ...:

  Arguments for slot gauge.

## Value

A \`billboard\` \`htmlwidget\` object.

## Note

This function can be used with \[billboarderProxy()\] in Shiny
applications.

## Examples

``` r
billboarder() %>%
  bb_gaugechart(value = 50)

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"json":{"Value":[50]},"type":"gauge"},"gauge":[],"color":{"pattern":["#FF0000","#F97600","#F6C600","#60B044"],"threshold":{"values":[30,60,90,100]}}},"data":null},"evals":[],"jsHooks":[]}
# With some options
billboarder() %>%
  bb_gaugechart(
    value = 160,
    steps_color = rev(c("#FF0000", "#F97600", "#F6C600", "#60B044"))
  ) %>%
  bb_gauge(
    label = list(format = suffix("km/h")),
    min = 10, max = 200, width = 20
  )

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"json":{"Value":[160]},"type":"gauge"},"gauge":{"label":{"format":"function(x) {return x + 'km/h';}"},"min":10,"max":200,"width":20},"color":{"pattern":["#60B044","#F6C600","#F97600","#FF0000"],"threshold":{"values":[30,60,90,100]}}},"data":null},"evals":["bb_opts.gauge.label.format"],"jsHooks":[]}
```
