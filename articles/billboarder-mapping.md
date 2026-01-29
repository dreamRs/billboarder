# Mapping - map variables to the chart

## Introduction

There’s 3 ways with `billboarder` to pass data to construct a chart :

- Use a `data.frame` with only variable that need to be used, typically
  for a barchart, the first column is used for x-axis, the second for
  y-axis.
- Pass your data to `billboarder` and use function `bb_aes` in a “pipe”
  flow.
- Use function `bbaes` inside chart builder helper such as
  `bb_barchart`, `bb_linechart`, …

## Data

We’ll use data about the french electricity production between 2012 and
2016 :

``` r
data("prod_par_filiere")
str(prod_par_filiere)
#> 'data.frame':    5 obs. of  11 variables:
#>  $ annee             : chr  "2012" "2013" "2014" "2015" ...
#>  $ prod_total        : num  542 550 540 547 531
#>  $ prod_therm        : num  48.1 43.6 25.9 34.4 45.9
#>  $ prod_hydraulique  : num  63.8 75.5 68.1 59.1 63.9
#>  $ prod_bioenergies  : num  5.8 7.1 7.5 8 8.5
#>  $ prod_eolien       : num  14.9 15.9 17.1 21.1 20.7
#>  $ prod_therm_charbon: num  17.4 19.9 8.4 8.6 7.3
#>  $ prod_solaire      : num  4.1 4.7 5.9 7.4 8.3
#>  $ prod_therm_gaz    : num  24 19.9 14.3 21.9 35.3
#>  $ prod_nucleaire    : num  405 404 416 417 384
#>  $ prod_therm_fioul  : num  6.7 3.8 3.3 3.8 3.3
```

## First method : use a `data.frame`

For creating this simple barchart, we need to use two columns of our
`data.frame`

``` r
billboarder() %>% 
  bb_barchart(data = prod_par_filiere[, c("annee", "prod_bioenergies")])
```

Variable `annee` is used on the x-axis, and `prod_bioenergies` as y
values.

This is similar for line chart :

``` r
billboarder() %>% 
  bb_linechart(data = prod_par_filiere[, c("annee", "prod_bioenergies")])
```

## Second method : with mapping

We can pass our data to function `billboarder` and then call `bb_aes` to
specify which variable to use :

``` r
billboarder(data = prod_par_filiere) %>% 
  bb_aes(x = annee, y = prod_bioenergies) %>% 
  bb_barchart()
```

You don’t have to pass arguments to `bb_barchart`.

This is the same for line chart :

``` r
billboarder(data = prod_par_filiere) %>% 
  bb_aes(x = annee, y = prod_bioenergies) %>% 
  bb_linechart()
```

## Third method : mapping inside function

Mapping can be specified inside the function which specify the type of
chart :

``` r
billboarder(data = prod_par_filiere) %>% 
  bb_barchart(mapping = bbaes(x = annee, y = prod_bioenergies))
```

The function to map variables is `bbaes` without underscore.

For line chart :

``` r
billboarder(data = prod_par_filiere) %>% 
  bb_linechart(mapping = bbaes(x = annee, y = prod_bioenergies))
```

## Grouping variable

Construct a chart with groups differ between the first method and the
others. For the first one, data need to be in ‘wide’ format, the other
need data in ‘long’ format with a grouping variable.

With ‘wide’ data :

``` r
billboarder() %>% 
  bb_barchart(
    data = prod_par_filiere[, c("annee", "prod_bioenergies", "prod_eolien", "prod_solaire", "prod_hydraulique")]
  )
```

with ‘long’ data :

``` r
# prepare data
data("prod_filiere_long")
prod_filiere_long <- prod_filiere_long[
  prod_filiere_long$branche %in% c("bioenergies", "eolien", "solaire", "hydraulique"), 
]
head(prod_filiere_long)
#>    annee     branche prod
#> 11  2012 hydraulique 63.8
#> 12  2013 hydraulique 75.5
#> 13  2014 hydraulique 68.1
#> 14  2015 hydraulique 59.1
#> 15  2016 hydraulique 63.9
#> 16  2012 bioenergies  5.8


billboarder(data = prod_filiere_long) %>% 
  bb_barchart(mapping = bbaes(x = annee, y = prod, group = branche))
```

## Mapping with programming

In Shiny app or in function, you can use `bbaes_string` or
`bb_aes_string`, these function accept character instead of unquoted
variable names :

``` r
billboarder(data = prod_filiere_long) %>% 
  bb_barchart(mapping = bbaes_string(x = "annee", y = "prod", group = "branche"))
#> Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
#> ℹ Please use tidy evaluation idioms with `aes()`.
#> ℹ See also `vignette("ggplot2-in-packages")` for more information.
#> ℹ The deprecated feature was likely used in the billboarder package.
#>   Please report the issue at <https://github.com/dreamRs/billboarder/issues>.
#> This warning is displayed once per session.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
```
