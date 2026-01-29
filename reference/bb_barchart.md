# Helper for creating a bar chart

Helper for creating a bar chart

## Usage

``` r
bb_barchart(
  bb,
  data,
  mapping = NULL,
  stacked = FALSE,
  rotated = FALSE,
  color = NULL,
  ...
)
```

## Arguments

- bb:

  A `billboard` `htmlwidget` object.

- data:

  A `data.frame`, the first column will be used for x axis unless
  specified otherwise in `mapping`. If not a `data.frame`, an object
  coercible to `data.frame`.

- mapping:

  Mapping of variables on the chart, see
  [`bbaes`](https://dreamrs.github.io/billboarder/reference/billboard-aes.md).

- stacked:

  Logical, if several columns are provided, produce a stacked bar chart,
  else a dodge bar chart.

- rotated:

  Switch x and y axis position.

- color:

  Bar's color.

- ...:

  Arguments for slot bar, see
  <https://naver.github.io/billboard.js/release/latest/doc/Options.html#.bar>.

## Value

A `billboard` `htmlwidget` object.

## Note

This function can be used with
[`billboarderProxy`](https://dreamrs.github.io/billboarder/reference/billboarder-shiny.md)
in shiny application.

## Examples

``` r
stars <- data.frame(
  package = c("billboarder", "ggiraph", "officer",
              "shinyWidgets", "visNetwork", "rAmCharts", 
              "D3partitionR"),
  stars = c(67, 252, 160, 144, 224, 32, 25)
)

# By default, first column is mapped on the x-axis
# second one on the y axis
billboarder() %>%
  bb_barchart(data = stars)

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"bb-x","json":{"bb-x":["billboarder","ggiraph","officer","shinyWidgets","visNetwork","rAmCharts","D3partitionR"],"stars":[67,252,160,144,224,32,25]},"type":"bar"},"bar":[],"axis":{"x":{"type":"category"},"rotated":false}},"data":null},"evals":[],"jsHooks":[]}

# Specify explicitly the columns to use
billboarder() %>%
  bb_barchart(data = stars, mapping = bbaes(package, stars), rotated = TRUE)

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"bb-x","json":{"bb-x":["billboarder","ggiraph","officer","shinyWidgets","visNetwork","rAmCharts","D3partitionR"],"stars":[67,252,160,144,224,32,25]},"type":"bar"},"bar":[],"axis":{"x":{"type":"category"},"rotated":true}},"data":null},"evals":[],"jsHooks":[]}

# Add some options
billboarder() %>%
  bb_barchart(data = stars[order(stars$stars), ], x = "package", y = "stars", rotated = TRUE) %>% 
  bb_data(names = list(stars = "Number of stars")) %>% 
  bb_y_grid(show = TRUE)

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"bb-x","json":{"bb-x":["D3partitionR","rAmCharts","billboarder","shinyWidgets","officer","visNetwork","ggiraph"],"stars":[25,32,67,144,160,224,252]},"type":"bar","names":{"stars":"Number of stars"}},"bar":{"x":"package","y":"stars"},"axis":{"x":{"type":"category"},"rotated":true},"grid":{"y":{"show":true}}},"data":null},"evals":[],"jsHooks":[]}


# Hack stacked barcharts (to color bar)
stars_wide <- data.frame(
  author = c("dreamRs", "davidgohel", "davidgohel", "dreamRs",
             "datastorm-open", "datastorm-open", "AntoineGuillot2"),
  package = c("billboarder", "ggiraph", "officer",
              "shinyWidgets", "visNetwork", "rAmCharts", 
              "D3partitionR"),
  stars = c(67, 252, 160, 144, 224, 32, 25)
)

billboarder() %>%
  bb_barchart(data = stars_wide, 
              mapping = bbaes(package, stars, group = author),
              stacked = TRUE)

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"bb-x","json":{"bb-x":["billboarder","ggiraph","officer","shinyWidgets","visNetwork","rAmCharts","D3partitionR"],"dreamRs":[67,null,null,144,null,null,null],"davidgohel":[null,252,160,null,null,null,null],"datastorm-open":[null,null,null,null,224,32,null],"AntoineGuillot2":[null,null,null,null,null,null,25]},"type":"bar","groups":[["dreamRs","davidgohel","datastorm-open","AntoineGuillot2"]]},"bar":[],"axis":{"x":{"type":"category"},"rotated":false}},"data":null},"evals":[],"jsHooks":[]}
billboarder() %>%
  bb_barchart(data = stars_wide,
              mapping = bbaes(author, stars, group = package),
              stacked = TRUE)

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"bb-x","json":{"bb-x":["dreamRs","davidgohel","datastorm-open","AntoineGuillot2"],"billboarder":[67,null,null,null],"ggiraph":[null,252,null,null],"officer":[null,160,null,null],"shinyWidgets":[144,null,null,null],"visNetwork":[null,null,224,null],"rAmCharts":[null,null,32,null],"D3partitionR":[null,null,null,25]},"type":"bar","groups":[["billboarder","ggiraph","officer","shinyWidgets","visNetwork","rAmCharts","D3partitionR"]]},"bar":[],"axis":{"x":{"type":"category"},"rotated":false}},"data":null},"evals":[],"jsHooks":[]}


# Grouping variable
tab <- table(sample(letters[1:5], 100, TRUE), sample(LETTERS[1:5], 100, TRUE))
dat <- as.data.frame(tab)

billboarder() %>%
  bb_barchart(data = dat, bbaes(x = Var1, y = Freq, group = Var2), rotated = TRUE)

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"bb-x","json":{"bb-x":["a","b","c","d","e"],"A":[5,3,4,3,5],"B":[4,4,6,4,6],"C":[4,4,1,6,5],"D":[5,5,3,5,5],"E":[4,5,4,0,0]},"type":"bar"},"bar":[],"axis":{"x":{"type":"category"},"rotated":true}},"data":null},"evals":[],"jsHooks":[]}

# You can also pass data in a 'wide' format
dat2 <- data.frame(
  x = letters[1:5],
  A = sample.int(n = 100, size = 5),
  B = sample.int(n = 100, size = 5),
  C = sample.int(n = 100, size = 5),
  D = sample.int(n = 100, size = 5),
  E = sample.int(n = 100, size = 5)
)

# But cannot use mapping
billboarder() %>%
  bb_barchart(data = dat2, stacked = TRUE) %>% 
  bb_data(order = NULL, labels = TRUE)

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"bb-x","json":{"bb-x":["a","b","c","d","e"],"A":[53,72,26,37,16],"B":[51,33,29,25,34],"C":[87,77,51,31,26],"D":[3,17,63,33,48],"E":[85,18,91,24,84]},"type":"bar","groups":[["A","B","C","D","E"]],"order":null,"labels":true},"bar":[],"axis":{"x":{"type":"category"},"rotated":false}},"data":null},"evals":[],"jsHooks":[]}
```
