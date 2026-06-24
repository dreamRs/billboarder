# Set categories on X axis

Set or modify x axis labels.

## Usage

``` r
bb_categories(bb, categories)
```

## Arguments

- bb:

  A \`billboard\` \`htmlwidget\` object.

- categories:

  A character vector to set names on a category axis.

## Value

A \`billboard\` \`htmlwidget\` object.

## Note

This function can be used with \`billboarder-shiny\` to modify labels on
axis, e.g. for barcharts.

## Examples

``` r
# Simple line with month names as x labels
billboarder() %>%
  bb_linechart(data = round(rnorm(12))) %>%
  bb_categories(categories = month.name)

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"line":{"classes":["billboarder-line-x"]},"data":{"x":"index","json":{"index":[1,2,3,4,5,6,7,8,9,10,11,12],"x":[0,0,0,-0,1,-1,1,-2,0,0,2,-1]},"types":{"x":"line"}},"point":{"show":false},"axis":{"x":{"type":"category","categories":["January","February","March","April","May","June","July","August","September","October","November","December"]}}},"data":null},"evals":[],"jsHooks":[]}
```
