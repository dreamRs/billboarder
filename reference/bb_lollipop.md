# Helper for creating a lollipop chart

Helper for creating a lollipop chart

## Usage

``` r
bb_lollipop(
  bb,
  data,
  mapping = NULL,
  rotated = FALSE,
  point_color = "#112446",
  point_size = 8,
  line_color = "#000",
  ...
)
```

## Arguments

- bb:

  A `billboard` `htmlwidget` object.

- data:

  A `data.frame`, the first column will be used for x axis unless
  argument `x` is specified, the second one will be use as y values. If
  not a `data.frame`, an object coercible to `data.frame`.

- mapping:

  Mapping of variables on the chart, see
  [`bbaes`](https://dreamrs.github.io/billboarder/reference/billboard-aes.md).

- rotated:

  Switch x and y axis position.

- point_color:

  Color of the lollipop.

- point_size:

  Size of the lollipop.

- line_color:

  Color of the lines between the axis and the lollipop.

- ...:

  Not used.

## Value

A `billboard` `htmlwidget` object.

## Examples

``` r

# From wikipedia
sw <- data.frame(
  film = c("The Force Awakens", "The Phantom Menace", 
           "Revenge of the Sith", "A New Hope",
           "Attack of the Clones", "The Empire Strikes Back",
           "Return of the Jedi"
  ),
  worldwide_gross = c(2068178225, 1027044677, 848754768,
                      775398007, 649398328, 538375067,
                      475106177)
)

# Simple example
billboarder() %>% 
  bb_lollipop(data = sw)

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"film","json":{"film":["The Force Awakens","The Phantom Menace","Revenge of the Sith","A New Hope","Attack of the Clones","The Empire Strikes Back","Return of the Jedi"],"worldwide_gross":[2068178225,1027044677,848754768,775398007,649398328,538375067,475106177],"lollipop":[2068178225,1027044677,848754768,775398007,649398328,538375067,475106177]},"type":"bar","classes":{"worldwide_gross":"lollipop-lines"},"types":{"lollipop":"bar","worldwide_gross":"line"},"colors":{"lollipop":"#000","worldwide_gross":"#112446"}},"axis":{"x":{"type":"category"},"rotated":false},"bar":{"width":1},"point":{"r":8},"legend":{"show":false},"customStyle":[".bb-target-lollipop-lines > .bb-circle{opacity: 1;}",".bb-target-lollipop-lines > .bb-lines{opacity: 0;}"],"tooltip":{"format":{"value":"function(value, ratio, id, index) {if (id !== 'lollipop') return value; }"}}},"data":null},"evals":["bb_opts.tooltip.format.value"],"jsHooks":[]}
# Fancy example
billboarder() %>% 
  bb_lollipop(data = sw, rotated = TRUE)%>% 
  bb_y_grid(show = TRUE) %>% 
  bb_y_axis(tick = list(
    values = c(0, 5e+08, 1e+09, 1.5e+09, 2e+09),
    outer = FALSE,
    format = htmlwidgets::JS("d3.formatPrefix('$,.0', 1e6)")
  )) %>% 
  bb_x_axis(tick = list(centered = TRUE)) %>% 
  bb_labs(
    title = "Star Wars - Total Lifetime Grosses",
    caption = "Data source : wikipedia"
  )

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"film","json":{"film":["The Force Awakens","The Phantom Menace","Revenge of the Sith","A New Hope","Attack of the Clones","The Empire Strikes Back","Return of the Jedi"],"worldwide_gross":[2068178225,1027044677,848754768,775398007,649398328,538375067,475106177],"lollipop":[2068178225,1027044677,848754768,775398007,649398328,538375067,475106177]},"type":"bar","classes":{"worldwide_gross":"lollipop-lines"},"types":{"lollipop":"bar","worldwide_gross":"line"},"colors":{"lollipop":"#000","worldwide_gross":"#112446"}},"axis":{"x":{"type":"category","tick":{"centered":true}},"rotated":true,"y":{"tick":{"values":[0,500000000,1000000000,1500000000,2000000000],"outer":false,"format":"d3.formatPrefix('$,.0', 1e6)"}}},"bar":{"width":1},"point":{"r":8},"legend":{"hide":true},"customStyle":[".bb-target-lollipop-lines > .bb-circle{opacity: 1;}",".bb-target-lollipop-lines > .bb-lines{opacity: 0;}",".bb-title tspan:nth-child(2){font-size: smaller; font-weight: lighter;}"],"tooltip":{"format":{"value":"function(value, ratio, id, index) {if (id !== 'lollipop') return value; }"}},"grid":{"y":{"show":true}},"padding":{"top":30},"title":{"text":"Star Wars - Total Lifetime Grosses\nData source : wikipedia","position":"left-top"}},"data":null},"evals":["bb_opts.axis.y.tick.format","bb_opts.tooltip.format.value"],"jsHooks":[]}

# With mapping
billboarder(data = sw) %>% 
  bb_lollipop(mapping = bbaes(x = film, y = worldwide_gross))

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"film","json":{"film":["The Force Awakens","The Phantom Menace","Revenge of the Sith","A New Hope","Attack of the Clones","The Empire Strikes Back","Return of the Jedi"],"worldwide_gross":[2068178225,1027044677,848754768,775398007,649398328,538375067,475106177],"lollipop":[2068178225,1027044677,848754768,775398007,649398328,538375067,475106177]},"type":"bar","classes":{"worldwide_gross":"lollipop-lines"},"types":{"lollipop":"bar","worldwide_gross":"line"},"colors":{"lollipop":"#000","worldwide_gross":"#112446"}},"axis":{"x":{"type":"category"},"rotated":false},"bar":{"width":1},"point":{"r":8},"legend":{"show":false},"customStyle":[".bb-target-lollipop-lines > .bb-circle{opacity: 1;}",".bb-target-lollipop-lines > .bb-lines{opacity: 0;}"],"tooltip":{"format":{"value":"function(value, ratio, id, index) {if (id !== 'lollipop') return value; }"}}},"data":{"film":["The Force Awakens","The Phantom Menace","Revenge of the Sith","A New Hope","Attack of the Clones","The Empire Strikes Back","Return of the Jedi"],"worldwide_gross":[2068178225,1027044677,848754768,775398007,649398328,538375067,475106177]}},"evals":["bb_opts.tooltip.format.value"],"jsHooks":[]}  
```
