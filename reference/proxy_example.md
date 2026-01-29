# Proxy use example

Launch an example to demonstrate how to use proxy method from
`billboarder` in Shiny app.

## Usage

``` r
proxy_example(chart = "gauge")
```

## Arguments

- chart:

  Chart type for which to see an example, possible values are `gauge`,
  `pie`, `bar`, `bar2`, `line`, `line2`, `density`, `histogram`,
  `lollipop`, `stacked_bar`.

## Examples

``` r
if (interactive()) {

# Titanic passenger
proxy_example("bar")

# Electricity production by sources and year
proxy_example("bar2")

# Moving lollipop with mpg dataset from ggplot2
proxy_example("lollipop")

# Update a stacked bar chart
proxy_example("stacked_bar")

# Moving sine and cosine
proxy_example("line")

# Changing lines and adding ones
proxy_example("line2")

# Update pie chart
proxy_example("pie")

# Density with ggplot2 diamonds
proxy_example("density")

# Histogram with ggplot2 diamonds
proxy_example("histogram")

}
```
