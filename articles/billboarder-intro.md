# Introduction - basic use

This package allow you to use
[billboard.js](https://naver.github.io/billboard.js/), a re-usable easy
interface JavaScript chart library, based on D3 v4+.

Supported chart types:

- line
- bar
- pie / donut
- scatter

The main function is `billboarder`, all charts begin with. You can add
layer to your charts with function `bb_*`, these functions correspond to
a billboard option define in the [API
docs](https://naver.github.io/billboard.js/release/latest/doc/). There
are helpers functions to quickly create a type of chart (`bb_barchart`,
`bb_linechart`, `bb_piechart`, `bb_donutchart`, `bb_gauge`,
`bb_scatterplot`), they have to be called after `billboarder`.

## Bar chart

You can create a simple bar chart by passing a `data.frame` to
`bb_barchart`, the first column will be used as the x-axis, and the
second one as the y-axis :

``` r

library("billboarder")

df <- as.data.frame(table(sample(letters[1:5], 50, TRUE)))
df
#>   Var1 Freq
#> 1    a    9
#> 2    b   11
#> 3    c   10
#> 4    d    9
#> 5    e   11

billboarder() %>% 
  bb_barchart(data = df)
```

If you want to create a grouped bar chart, first option is to put your
data in a “wide” format. Here we use
[`stats::reshape`](https://rdrr.io/r/stats/reshape.html), but I
recommend to use `tidyr::spread` or `data.table::dcast`.

``` r

df <- as.data.frame(table(
  sample(letters[1:5], 50, TRUE),
  sample(LETTERS[1:5], 50, TRUE)
))
df.r <- reshape(data = df, idvar = "Var1", timevar = "Var2", direction = "wide")
df.r
#>   Var1 Freq.A Freq.B Freq.C Freq.D Freq.E
#> 1    a      2      5      2      1      4
#> 2    b      3      1      3      2      0
#> 3    c      2      1      1      2      2
#> 4    d      2      2      0      2      3
#> 5    e      2      1      2      4      1

billboarder() %>% 
  bb_barchart(data = df.r)
```

Second option is to define a mapping of your variable with function
`bbaes` (for more example of mapping, see vignette
*billboarder-mapping*).

``` r

billboarder() %>% 
  bb_barchart(
    data = df,
    mapping = bbaes(x = Var1, y = Freq, group = Var2)
  )
```

## Line chart

You can pass to the function `bb_linechart` a vector, in that case
x-axis will be the index of that vector :

``` r

billboarder() %>% 
  bb_linechart(data = sin(seq(-pi, pi, length.out = 10)))
```

You can change the type of line with argument `type`, for example an
`area-step` :

``` r

billboarder() %>% 
  bb_linechart(data = sin(seq(-pi, pi, length.out = 10)), type = "area-step")
```

If want to specify a variable to map to the x-axis, you had to pass a
`data.frame` to the function :

``` r

df <- data.frame(
  var_x = seq(-pi, pi, length.out = 10),
  sin = sin(seq(-pi, pi, length.out = 10))
)
df
#>         var_x           sin
#> 1  -3.1415927 -1.224647e-16
#> 2  -2.4434610 -6.427876e-01
#> 3  -1.7453293 -9.848078e-01
#> 4  -1.0471976 -8.660254e-01
#> 5  -0.3490659 -3.420201e-01
#> 6   0.3490659  3.420201e-01
#> 7   1.0471976  8.660254e-01
#> 8   1.7453293  9.848078e-01
#> 9   2.4434610  6.427876e-01
#> 10  3.1415927  1.224647e-16

billboarder() %>% 
  bb_linechart(data = df, x = "var_x")
```

If the first variable of the `data.frame` is a `Date` or a `POSIX`, it
will be automatically mapped to the x-axis :

``` r

df <- data.frame(
  date = seq.Date(from = as.Date("2017-06-12"), by = "day", length.out = 10),
  var = rnorm(10)
)
df
#>          date        var
#> 1  2017-06-12  2.1268505
#> 2  2017-06-13  0.4248584
#> 3  2017-06-14 -1.6842815
#> 4  2017-06-15  0.2494018
#> 5  2017-06-16  1.0728383
#> 6  2017-06-17  2.0393693
#> 7  2017-06-18  0.4494538
#> 8  2017-06-19  1.3918140
#> 9  2017-06-20  0.4265665
#> 10 2017-06-21  0.1075840

billboarder() %>% 
  bb_linechart(data = df)
```

## Scatter plot

For scatter plot, use a two column `data.frame` with function
`bb_scatterplot`, or specify the x variable and the y variable (you can
also specify a grouping variable) :

``` r

billboarder() %>% 
  bb_scatterplot(data = iris[, 1:2])
```

``` r


billboarder() %>% 
  bb_scatterplot(data = iris, x = "Petal.Length", y = "Petal.Width", group = "Species")
```

## Pie chart

For pie chart, use `bb_piechart` with a two column `data.frame` :

``` r

df <- data.frame(
  var = c("A", "B"),
  count = c(457, 987)
)

billboarder() %>% 
  bb_piechart(data = df)
```

## Donut chart

Donut charts works the same as pie charts :

``` r

df <- data.frame(
  var = c("A", "B"),
  count = c(687, 246)
)

billboarder() %>% 
  bb_donutchart(data = df)
```

Note : pie and donut are automatically sorted, you can change that with
`bb_data(order = NULL)`.

## Gauge chart

Gauge only need one value :

``` r

billboarder() %>% 
  bb_gaugechart(value = 64)
```
