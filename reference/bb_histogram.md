# Helper for creating an histogram

Helper for creating an histogram

## Usage

``` r
bb_histogram(
  bb,
  data,
  mapping = NULL,
  stacked = FALSE,
  fill = FALSE,
  bins = 30,
  binwidth = NULL,
  ...
)
```

## Arguments

- bb:

  A \`billboard\` \`htmlwidget\` object.

- data:

  A \`data.frame\` or a \`vector\`, the first column will be used to
  calculate density if \`x\` is \`NULL\`.

- mapping:

  Mapping of variables on the chart, see \[bbaes()\].

- stacked:

  Logical, create a stacked histogram.

- fill:

  Logical, create a stacked percentage histogram.

- bins:

  Number of bins. Overridden by \`binwidth\`. Defaults to 30.

- binwidth:

  The width of the bins. See \[ggplot2::geom_histogram()\]

- ...:

  Not used.

## Value

A \`billboard\` \`htmlwidget\` object.

## See also

\[bb_densityplot()\]

## Examples

``` r
data("diamonds", package = "ggplot2")

# one variable
billboarder() %>%
  bb_histogram(data = diamonds, x = "price")
#> Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
#> ℹ Please use tidy evaluation idioms with `aes()`.
#> ℹ See also `vignette("ggplot2-in-packages")` for more information.
#> ℹ The deprecated feature was likely used in the billboarder package.
#>   Please report the issue at <https://github.com/dreamRs/billboarder/issues>.

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"x","json":{"x":[326,963.8275862068965,1601.655172413793,2239.482758620689,2877.310344827586,3515.137931034483,4152.965517241379,4790.793103448275,5428.620689655172,6066.448275862069,6704.275862068966,7342.103448275861,7979.931034482758,8617.758620689656,9255.586206896551,9893.413793103449,10531.24137931034,11169.06896551724,11806.89655172414,12444.72413793104,13082.55172413793,13720.37931034483,14358.20689655172,14996.03448275862,15633.86206896552,16271.68965517241,16909.51724137931,17547.3448275862,18185.1724137931,18823],"y":[5285,13140,5201,4268,3310,2669,3001,2782,2052,1638,1350,1142,891,827,742,632,592,574,465,448,426,362,349,306,314,281,297,232,262,102]},"type":"area-step"},"legend":{"show":false},"axis":{"x":{"label":{"text":"price"},"tick":{"fit":false,"outer":false,"centered":true}},"y":{"max":null,"padding":null,"tick":{"format":null},"label":{"text":"count"}}},"tooltip":{"format":{"title":"function(i) { var x = (i-318.914) + ' ; ' + (i+318.914); return x; }"}},"point":{"show":false}},"data":null},"evals":["bb_opts.tooltip.format.title"],"jsHooks":[]}
# with mapping
billboarder() %>%
  bb_histogram(diamonds, bbaes(price))

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"x","json":{"x":[326,963.8275862068965,1601.655172413793,2239.482758620689,2877.310344827586,3515.137931034483,4152.965517241379,4790.793103448275,5428.620689655172,6066.448275862069,6704.275862068966,7342.103448275861,7979.931034482758,8617.758620689656,9255.586206896551,9893.413793103449,10531.24137931034,11169.06896551724,11806.89655172414,12444.72413793104,13082.55172413793,13720.37931034483,14358.20689655172,14996.03448275862,15633.86206896552,16271.68965517241,16909.51724137931,17547.3448275862,18185.1724137931,18823],"y":[5285,13140,5201,4268,3310,2669,3001,2782,2052,1638,1350,1142,891,827,742,632,592,574,465,448,426,362,349,306,314,281,297,232,262,102]},"type":"area-step"},"legend":{"show":false},"axis":{"x":{"label":{"text":"price"},"tick":{"fit":false,"outer":false,"centered":true}},"y":{"max":null,"padding":null,"tick":{"format":null},"label":{"text":"count"}}},"tooltip":{"format":{"title":"function(i) { var x = (i-318.914) + ' ; ' + (i+318.914); return x; }"}},"point":{"show":false}},"data":null},"evals":["bb_opts.tooltip.format.title"],"jsHooks":[]}
# equivalent to
billboarder() %>%
  bb_histogram(data = diamonds$price)

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"x","json":{"x":[326,963.8275862068965,1601.655172413793,2239.482758620689,2877.310344827586,3515.137931034483,4152.965517241379,4790.793103448275,5428.620689655172,6066.448275862069,6704.275862068966,7342.103448275861,7979.931034482758,8617.758620689656,9255.586206896551,9893.413793103449,10531.24137931034,11169.06896551724,11806.89655172414,12444.72413793104,13082.55172413793,13720.37931034483,14358.20689655172,14996.03448275862,15633.86206896552,16271.68965517241,16909.51724137931,17547.3448275862,18185.1724137931,18823],"y":[5285,13140,5201,4268,3310,2669,3001,2782,2052,1638,1350,1142,891,827,742,632,592,574,465,448,426,362,349,306,314,281,297,232,262,102]},"type":"area-step"},"legend":{"show":false},"axis":{"x":{"label":{"text":"x"},"tick":{"fit":false,"outer":false,"centered":true}},"y":{"max":null,"padding":null,"tick":{"format":null},"label":{"text":"count"}}},"tooltip":{"format":{"title":"function(i) { var x = (i-318.914) + ' ; ' + (i+318.914); return x; }"}},"point":{"show":false}},"data":null},"evals":["bb_opts.tooltip.format.title"],"jsHooks":[]}
# prettier with 'binwidth'
# (but you need to know your data)
billboarder() %>%
  bb_histogram(data = diamonds, x = "price", binwidth = 500) %>%
  bb_colors_manual()

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"x","json":{"x":[500,1000,1500,2000,2500,3000,3500,4000,4500,5000,5500,6000,6500,7000,7500,8000,8500,9000,9500,10000,10500,11000,11500,12000,12500,13000,13500,14000,14500,15000,15500,16000,16500,17000,17500,18000,18500,19000],"y":[8428,9696,3906,3752,3180,2479,2076,2246,2422,2008,1522,1293,1172,944,861,676,634,629,550,491,466,481,388,349,351,339,289,282,272,238,221,261,219,233,178,207,178,23]},"type":"area-step","colors":[]},"legend":{"show":false},"axis":{"x":{"label":{"text":"price"},"tick":{"fit":false,"outer":false,"centered":true}},"y":{"max":null,"padding":null,"tick":{"format":null},"label":{"text":"count"}}},"tooltip":{"format":{"title":"function(i) { var x = (i-250) + ' ; ' + (i+250); return x; }"}},"point":{"show":false},"billboarderspecials":{"opacity":1}},"data":null},"evals":["bb_opts.tooltip.format.title"],"jsHooks":[]}
# with a grouping variable
billboarder() %>%
  bb_histogram(data = diamonds, x = "price",
               group = "cut", binwidth = 500)

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"x","json":{"x":[500,1000,1500,2000,2500,3000,3500,4000,4500,5000,5500,6000,6500,7000,7500,8000,8500,9000,9500,10000,10500,11000,11500,12000,12500,13000,13500,14000,14500,15000,15500,16000,16500,17000,17500,18000,18500,19000],"Fair":[48,162,99,161,159,167,113,122,104,77,49,38,39,32,27,21,17,14,10,11,12,19,8,5,12,10,14,9,6,6,10,7,4,3,4,6,5,0],"Good":[836,502,341,335,310,232,299,368,333,198,143,142,101,96,73,61,52,53,36,36,26,28,30,22,22,27,24,23,17,21,16,25,21,8,16,16,16,1],"Very Good":[2352,1411,836,780,791,595,483,573,661,494,360,309,265,240,209,144,132,139,120,124,106,81,91,83,75,74,57,53,55,53,45,66,47,47,36,56,34,5],"Premium":[1693,2453,798,742,716,544,465,631,727,612,497,427,361,245,265,203,191,184,161,149,140,162,130,113,114,122,109,108,101,91,75,92,68,102,66,65,62,7],"Ideal":[3499,5168,1832,1734,1204,941,716,552,597,627,473,377,406,331,287,247,242,239,223,171,182,191,129,126,128,106,85,89,93,67,75,71,79,73,56,64,61,10]},"type":"area-step"},"legend":{"show":true},"axis":{"x":{"label":{"text":"price"},"tick":{"fit":false,"outer":false,"centered":true}},"y":{"max":null,"padding":null,"tick":{"format":null},"label":{"text":"count"}}},"tooltip":{"format":{"title":"function(i) { var x = (i-250) + ' ; ' + (i+250); return x; }"}},"point":{"show":false}},"data":null},"evals":["bb_opts.tooltip.format.title"],"jsHooks":[]}
# and with mapping
billboarder() %>%
  bb_histogram(diamonds, bbaes(price, group = cut),
               binwidth = 500)

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"x","json":{"x":[500,1000,1500,2000,2500,3000,3500,4000,4500,5000,5500,6000,6500,7000,7500,8000,8500,9000,9500,10000,10500,11000,11500,12000,12500,13000,13500,14000,14500,15000,15500,16000,16500,17000,17500,18000,18500,19000],"Fair":[48,162,99,161,159,167,113,122,104,77,49,38,39,32,27,21,17,14,10,11,12,19,8,5,12,10,14,9,6,6,10,7,4,3,4,6,5,0],"Good":[836,502,341,335,310,232,299,368,333,198,143,142,101,96,73,61,52,53,36,36,26,28,30,22,22,27,24,23,17,21,16,25,21,8,16,16,16,1],"Very Good":[2352,1411,836,780,791,595,483,573,661,494,360,309,265,240,209,144,132,139,120,124,106,81,91,83,75,74,57,53,55,53,45,66,47,47,36,56,34,5],"Premium":[1693,2453,798,742,716,544,465,631,727,612,497,427,361,245,265,203,191,184,161,149,140,162,130,113,114,122,109,108,101,91,75,92,68,102,66,65,62,7],"Ideal":[3499,5168,1832,1734,1204,941,716,552,597,627,473,377,406,331,287,247,242,239,223,171,182,191,129,126,128,106,85,89,93,67,75,71,79,73,56,64,61,10]},"type":"area-step"},"legend":{"show":true},"axis":{"x":{"label":{"text":"price"},"tick":{"fit":false,"outer":false,"centered":true}},"y":{"max":null,"padding":null,"tick":{"format":null},"label":{"text":"count"}}},"tooltip":{"format":{"title":"function(i) { var x = (i-250) + ' ; ' + (i+250); return x; }"}},"point":{"show":false}},"data":null},"evals":["bb_opts.tooltip.format.title"],"jsHooks":[]}
# stacked histogram
billboarder() %>%
  bb_histogram(diamonds, bbaes(price, group = cut),
               stacked = TRUE, binwidth = 500)

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"x","json":{"x":[500,1000,1500,2000,2500,3000,3500,4000,4500,5000,5500,6000,6500,7000,7500,8000,8500,9000,9500,10000,10500,11000,11500,12000,12500,13000,13500,14000,14500,15000,15500,16000,16500,17000,17500,18000,18500,19000],"Fair":[48,162,99,161,159,167,113,122,104,77,49,38,39,32,27,21,17,14,10,11,12,19,8,5,12,10,14,9,6,6,10,7,4,3,4,6,5,0],"Good":[836,502,341,335,310,232,299,368,333,198,143,142,101,96,73,61,52,53,36,36,26,28,30,22,22,27,24,23,17,21,16,25,21,8,16,16,16,1],"Very Good":[2352,1411,836,780,791,595,483,573,661,494,360,309,265,240,209,144,132,139,120,124,106,81,91,83,75,74,57,53,55,53,45,66,47,47,36,56,34,5],"Premium":[1693,2453,798,742,716,544,465,631,727,612,497,427,361,245,265,203,191,184,161,149,140,162,130,113,114,122,109,108,101,91,75,92,68,102,66,65,62,7],"Ideal":[3499,5168,1832,1734,1204,941,716,552,597,627,473,377,406,331,287,247,242,239,223,171,182,191,129,126,128,106,85,89,93,67,75,71,79,73,56,64,61,10]},"type":"area-step","groups":[["Ideal","Premium","Good","Very Good","Fair"]]},"legend":{"show":true},"axis":{"x":{"label":{"text":"price"},"tick":{"fit":false,"outer":false,"centered":true}},"y":{"max":null,"padding":null,"tick":{"format":null},"label":{"text":"count"}}},"tooltip":{"format":{"title":"function(i) { var x = (i-250) + ' ; ' + (i+250); return x; }"}},"point":{"show":false}},"data":null},"evals":["bb_opts.tooltip.format.title"],"jsHooks":[]}
# another example
dat <- data.frame(
  sample = c(rnorm(n = 500, mean = 1), rnorm(n = 500, mean = 2)),
  group = rep(c("A", "B"), each = 500)
)

billboarder() %>%
  bb_histogram(data = dat, x = "sample", binwidth = 0.25)

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"x","json":{"x":[-2.75,-2.5,-2.25,-2,-1.75,-1.5,-1.25,-1,-0.75,-0.5,-0.25,0,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5,3.75,4,4.25,4.5,4.75,5,5.25,5.5],"y":[1,0,0,1,0,2,4,7,10,20,30,37,43,57,72,72,79,84,102,93,74,55,48,38,26,20,9,11,1,0,2,1,0,1]},"type":"area-step"},"legend":{"show":false},"axis":{"x":{"label":{"text":"sample"},"tick":{"fit":false,"outer":false,"centered":true}},"y":{"max":null,"padding":null,"tick":{"format":null},"label":{"text":"count"}}},"tooltip":{"format":{"title":"function(i) { var x = (i-0.125) + ' ; ' + (i+0.125); return x; }"}},"point":{"show":false}},"data":null},"evals":["bb_opts.tooltip.format.title"],"jsHooks":[]}
samples_mean <- tapply(dat$sample, dat$group, mean)
billboarder() %>%
  bb_histogram(data = dat, x = "sample", group = "group",
               binwidth = 0.25) %>%
  bb_x_grid(
    lines = list(
      list(value = unname(samples_mean["A"]),
           text = "mean of sample A"),
      list(value = unname(samples_mean["B"]),
           text = "mean of sample B")
    )
  )

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"data":{"x":"x","json":{"A":[1,0,0,1,0,2,4,5,10,18,27,29,35,38,46,48,38,39,49,38,26,16,10,8,7,3,1,1,0,0,0,0,0,0],"B":[0,0,0,0,0,0,0,2,0,2,3,8,8,19,26,24,41,45,53,55,48,39,38,30,19,17,8,10,1,0,2,1,0,1],"x":[-2.75,-2.5,-2.25,-2,-1.75,-1.5,-1.25,-1,-0.75,-0.5,-0.25,0,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5,3.75,4,4.25,4.5,4.75,5,5.25,5.5]},"type":"area-step"},"legend":{"show":true},"axis":{"x":{"label":{"text":"sample"},"tick":{"fit":false,"outer":false,"centered":true}},"y":{"max":null,"padding":null,"tick":{"format":null},"label":{"text":"count"}}},"tooltip":{"format":{"title":"function(i) { var x = (i-0.125) + ' ; ' + (i+0.125); return x; }"}},"point":{"show":false},"grid":{"x":{"lines":[{"value":1.056265709230094,"text":"mean of sample A"},{"value":1.972482740874396,"text":"mean of sample B"}]}}},"data":null},"evals":["bb_opts.tooltip.format.title"],"jsHooks":[]}
```
