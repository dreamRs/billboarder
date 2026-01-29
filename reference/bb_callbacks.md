# Callbacks for billboard charts

Callbacks for billboard charts

## Usage

``` r
bb_callbacks(
  bb,
  onafterinit = NULL,
  onbeforeinit = NULL,
  oninit = NULL,
  onout = NULL,
  onover = NULL,
  onrendered = NULL,
  onresize = NULL,
  onresized = NULL
)
```

## Arguments

- bb:

  A `billboard` `htmlwidget` object.

- onafterinit:

  Set a callback to execute after the chart is initialized.

- onbeforeinit:

  Set a callback to execute before the chart is initialized.

- oninit:

  Set a callback to execute when the chart is initialized.

- onout:

  Set a callback to execute when mouse/touch leaves the chart.

- onover:

  Set a callback to execute when mouse/touch enters the chart.

- onrendered:

  Set a callback which is executed when the chart is rendered.
  Basically, this callback will be called in each time when the chart is
  redrawed.

- onresize:

  Set a callback to execute when user resizes the screen.

- onresized:

  Set a callback to execute when screen resize finished.

## Value

A `billboard` `htmlwidget` object.

## Note

Set JavaScript callbacks for various billboard events. See the
[billboard
options](https://naver.github.io/billboard.js/release/latest/doc/Options.html)
reference for additional details on the signature of each callback.
