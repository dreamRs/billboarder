# Map variables on the chart

Map variables on the chart

## Usage

``` r
bb_aes(bb, ...)

bb_aes_string(bb, ...)

bbaes(...)

bbaes_string(...)
```

## Arguments

- bb:

  A `billboard` `htmlwidget` object.

- ...:

  Mapping parameters, such as `x` for x-axis, `y` for y-axis, `group`
  for grouping variable.

## Value

A `billboard` `htmlwidget` object.

## Note

`bb_aes` is intended to use in a "piping" way. `bbaes` is the equivalent
to use inside a helper function such as `bb_barchart`,
`bb_scatterplot`...

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- as.data.frame(table(sample(letters[1:5], 100, TRUE)))

billboarder(data = dat) %>% 
  bb_aes(x = Var1, y = Freq) %>% 
  bb_barchart()


tab <- table(sample(letters[1:5], 100, TRUE), sample(LETTERS[1:5], 100, TRUE))
dat_group <- as.data.frame(tab)

billboarder(data = dat_group) %>% 
  bb_aes(x = Var1, y = Freq, group = "Var2") %>% 
  bb_barchart()
} # }
```
