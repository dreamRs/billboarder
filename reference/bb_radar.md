# Radar property for a Billboard.js chart

Radar property for a Billboard.js chart

## Usage

``` r
bb_radar(bb, ...)
```

## Arguments

- bb:

  A \`billboard\` \`htmlwidget\` object.

- ...:

  See
  \<https://naver.github.io/billboard.js/release/latest/doc/Options.html#.radar\>

## Value

A \`billboard\` \`htmlwidget\` object.

## Examples

``` r
library("billboarder")
data("avengers")

# number of levels
billboarder() %>%
  bb_radarchart(
    data = avengers,
    mapping = bbaes(x = axis, y = value, group = group)
  ) %>%
  bb_radar(level = list(depth = 4))

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"bb-x","json":{"bb-x":["Intelligence","Strength","Speed","Durability","Energy","Fighting Skills"],"Captain America":[3,3,2,3,1,6],"Iron Man":[6,6,5,6,6,4],"Hulk":[6,7,3,7,1,4],"Thor":[2,7,7,6,6,4]},"type":"radar"},"radar":{"level":{"depth":4}}},"data":null},"evals":[],"jsHooks":[]}
# hide levels
billboarder() %>%
  bb_radarchart(
    data = avengers,
    mapping = bbaes(x = axis, y = value, group = group)
  ) %>%
  bb_radar(level = list(show = FALSE))

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"bb-x","json":{"bb-x":["Intelligence","Strength","Speed","Durability","Energy","Fighting Skills"],"Captain America":[3,3,2,3,1,6],"Iron Man":[6,6,5,6,6,4],"Hulk":[6,7,3,7,1,4],"Thor":[2,7,7,6,6,4]},"type":"radar"},"radar":{"level":{"show":false}}},"data":null},"evals":[],"jsHooks":[]}
# max value on axis
billboarder() %>%
  bb_radarchart(
    data = avengers,
    mapping = bbaes(x = axis, y = value, group = group)
  ) %>%
  bb_radar(axis = list(max = 10))

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"bb-x","json":{"bb-x":["Intelligence","Strength","Speed","Durability","Energy","Fighting Skills"],"Captain America":[3,3,2,3,1,6],"Iron Man":[6,6,5,6,6,4],"Hulk":[6,7,3,7,1,4],"Thor":[2,7,7,6,6,4]},"type":"radar"},"radar":{"axis":{"max":10}}},"data":null},"evals":[],"jsHooks":[]}
```
