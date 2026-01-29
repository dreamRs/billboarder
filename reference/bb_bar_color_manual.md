# Manual color for barchart

Manual color for barchart

## Usage

``` r
bb_bar_color_manual(bb, values)
```

## Arguments

- bb:

  A `billboard` `htmlwidget` object.

- values:

  A named vector, names represent the categories of the bar chart,
  values correspond to colors. All categories must be present in the
  vector, in the same order of the chart.

## Value

A `billboard` `htmlwidget` object.

## Note

Must be called after `bb_bar`.

## Examples

``` r
if (FALSE) { # \dontrun{

library("data.table")
library("billboarder")

data("mpg", package = "ggplot2")
setDT(mpg)

# all in blue
manufa <- unique(mpg$manufacturer)
cols <- rep("#08298A", length(manufa))
names(cols) <- manufa

# Nissan in red
cols[["nissan"]] <- "#DF0101"#' 

billboarder() %>%
  bb_barchart(data = mpg[, list(count = .N), by = manufacturer][order(count)]) %>%
  bb_bar_color_manual(values = cols)
} # }
```
