# Add title to Billboard.js chart

Add title to Billboard.js chart

## Usage

``` r
bb_title(bb, text = NULL, padding = NULL, position = "top-center", ...)
```

## Arguments

- bb:

  A `billboard` `htmlwidget` object.

- text:

  The chart title.

- padding:

  A named list with `top`, `right`, `bottom`, `left` values.

- position:

  A string specifying the position of the title.

- ...:

  Additional arguments.

## Value

A `billboard` `htmlwidget` object.

## See also

[bb_labs](https://dreamrs.github.io/billboarder/reference/bb_labs.md)

## Examples

``` r
billboarder() %>% 
  bb_barchart(data = table(sample(letters, 100, TRUE))) %>% 
  bb_title(text = "Random letters", position = "center")

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"bb-x","json":{"bb-x":["b","c","d","e","f","g","h","i","j","k","l","n","o","p","q","r","s","t","u","v","w","x","y","z"],"Freq":[1,2,1,4,1,8,5,6,5,2,6,3,5,4,8,4,3,3,8,5,3,3,6,4]},"type":"bar"},"bar":[],"axis":{"x":{"type":"category"},"rotated":false},"title":{"text":"Random letters","position":"center"}},"data":null},"evals":[],"jsHooks":[]}
```
