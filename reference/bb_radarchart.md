# Helper for creating a radar chart

Helper for creating a radar chart

## Usage

``` r
bb_radarchart(bb, data, mapping = NULL, ...)
```

## Arguments

- bb:

  A \`billboard\` \`htmlwidget\` object.

- data:

  A \`data.frame\`, the first column will be used for x axis unless
  specified otherwise in \`mapping\`. If not a \`data.frame\`, an object
  coercible to \`data.frame\`.

- mapping:

  Mapping of variables on the chart, see \[bbaes()\].

- ...:

  Arguments passed to \[bb_radar()\].

## Value

A \`billboard\` \`htmlwidget\` object.

## Examples

``` r
library("billboarder")

# data about Avengers
data("avengers_wide")

# if not specified, first column is used as x-axis,
# all others are used on y-axis
billboarder() %>%
  bb_radarchart(data = avengers_wide)

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"bb-x","json":{"bb-x":["Durability","Energy","Fighting Skills","Intelligence","Speed","Strength"],"Captain America":[3,1,6,3,2,3],"Hulk":[7,1,4,6,3,7],"Iron Man":[6,6,4,6,5,6],"Thor":[6,6,4,2,7,7]},"type":"radar"},"radar":[]},"data":null},"evals":[],"jsHooks":[]}
# specify explicitly which column to use with mapping
billboarder() %>%
  bb_radarchart(
    data = avengers_wide,
    mapping = bbaes(x = axis, y = `Captain America`)
  )

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"bb-x","json":{"bb-x":["Durability","Energy","Fighting Skills","Intelligence","Speed","Strength"],"Captain America":[3,1,6,3,2,3]},"type":"radar"},"radar":[]},"data":null},"evals":[],"jsHooks":[]}
# with data in "long" format you can use "group" aesthetics
data("avengers")
billboarder() %>%
  bb_radarchart(
    data = avengers,
    mapping = bbaes(x = axis, y = value, group = group)
  )

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"bb-x","json":{"bb-x":["Intelligence","Strength","Speed","Durability","Energy","Fighting Skills"],"Captain America":[3,3,2,3,1,6],"Iron Man":[6,6,5,6,6,4],"Hulk":[6,7,3,7,1,4],"Thor":[2,7,7,6,6,4]},"type":"radar"},"radar":[]},"data":null},"evals":[],"jsHooks":[]}
```
