# Set colors for each datas

Set colors for each datas

## Usage

``` r
bb_colors_manual(bb, ..., opacity = 1)
```

## Arguments

- bb:

  A \`billboard\` \`htmlwidget\` object.

- ...:

  A named list, where names correspond to the data, and values to color
  associate with it.

- opacity:

  Color opacity (for area charts).

## Value

A \`billboard\` \`htmlwidget\` object.

## Examples

``` r
library("RColorBrewer")

# Scatter
billboarder() %>%
  bb_scatterplot(
    data = iris,
    x = "Sepal.Length",
    y = "Sepal.Width",
    group = "Species"
  ) %>%
  bb_axis(x = list(tick = list(fit = FALSE))) %>%
  bb_point(r = 8) %>%
  bb_colors_manual(
    setosa = "#440154",
    virginica = "#21908C",
    versicolor = "#FDE725"
  )

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"xs":{"setosa":"setosa_x","versicolor":"versicolor_x","virginica":"virginica_x"},"json":{"setosa_x":[5.1,4.9,4.7,4.6,5,5.4,4.6,5,4.4,4.9,5.4,4.8,4.8,4.3,5.8,5.7,5.4,5.1,5.7,5.1,5.4,5.1,4.6,5.1,4.8,5,5,5.2,5.2,4.7,4.8,5.4,5.2,5.5,4.9,5,5.5,4.9,4.4,5.1,5,4.5,4.4,5,5.1,4.8,5.1,4.6,5.3,5],"versicolor_x":[7,6.4,6.9,5.5,6.5,5.7,6.3,4.9,6.6,5.2,5,5.9,6,6.1,5.6,6.7,5.6,5.8,6.2,5.6,5.9,6.1,6.3,6.1,6.4,6.6,6.8,6.7,6,5.7,5.5,5.5,5.8,6,5.4,6,6.7,6.3,5.6,5.5,5.5,6.1,5.8,5,5.6,5.7,5.7,6.2,5.1,5.7],"virginica_x":[6.3,5.8,7.1,6.3,6.5,7.6,4.9,7.3,6.7,7.2,6.5,6.4,6.8,5.7,5.8,6.4,6.5,7.7,7.7,6,6.9,5.6,7.7,6.3,6.7,7.2,6.2,6.1,6.4,7.2,7.4,7.9,6.4,6.3,6.1,7.7,6.3,6.4,6,6.9,6.7,6.9,5.8,6.8,6.7,6.7,6.3,6.5,6.2,5.9],"setosa":[3.5,3,3.2,3.1,3.6,3.9,3.4,3.4,2.9,3.1,3.7,3.4,3,3,4,4.4,3.9,3.5,3.8,3.8,3.4,3.7,3.6,3.3,3.4,3,3.4,3.5,3.4,3.2,3.1,3.4,4.1,4.2,3.1,3.2,3.5,3.6,3,3.4,3.5,2.3,3.2,3.5,3.8,3,3.8,3.2,3.7,3.3],"versicolor":[3.2,3.2,3.1,2.3,2.8,2.8,3.3,2.4,2.9,2.7,2,3,2.2,2.9,2.9,3.1,3,2.7,2.2,2.5,3.2,2.8,2.5,2.8,2.9,3,2.8,3,2.9,2.6,2.4,2.4,2.7,2.7,3,3.4,3.1,2.3,3,2.5,2.6,3,2.6,2.3,2.7,3,2.9,2.9,2.5,2.8],"virginica":[3.3,2.7,3,2.9,3,3,2.5,2.9,2.5,3.6,3.2,2.7,3,2.5,2.8,3.2,3,3.8,2.6,2.2,3.2,2.8,2.8,2.7,3.3,3.2,2.8,3,2.8,3,2.8,3.8,2.8,2.8,2.6,3,3.4,3.1,3,3.1,3.1,3.1,2.7,3.2,3.3,3,2.5,3,3.4,3]},"type":"scatter","colors":{"setosa":"#440154","virginica":"#21908C","versicolor":"#FDE725"}},"legend":{"show":true},"axis":{"x":{"label":{"text":"Sepal.Length"},"tick":{"fit":false}},"y":{"label":{"text":"Sepal.Width"}}},"point":{"r":8},"billboarderspecials":{"opacity":1}},"data":null},"evals":[],"jsHooks":[]}
# Pie
stars <- data.frame(
  package = c("billboarder", "ggiraph", "officer",
              "shinyWidgets", "visNetwork"),
  stars = c(9, 177, 43, 44, 169)
)
cols <- brewer.pal(n = 5, name = "Dark2")

billboarder() %>%
  bb_piechart(data = stars) %>%
  bb_colors_manual(
    setNames(as.list(cols), stars$package) # this is a named list
  )

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"json":{"billboarder":[9],"ggiraph":[177],"officer":[43],"shinyWidgets":[44],"visNetwork":[169]},"type":"pie","colors":{"billboarder":"#1B9E77","ggiraph":"#D95F02","officer":"#7570B3","shinyWidgets":"#E7298A","visNetwork":"#66A61E"}},"pie":[],"billboarderspecials":{"opacity":1}},"data":null},"evals":[],"jsHooks":[]}
```
