
context("bb_add_style")

test_that("bb_add_style works", {
  
  
  bb <- billboarder() %>% 
    bb_add_style(
      region = list(region1 = "fill:red"),
      x_grid = list(x1 = "stroke:red"),
      y_grid = list(y1 = "stroke:blue"),
      "bb-line" = "stroke: green",
      .list = list("bb-line" = "stroke: green")
    )
  
  expect_is(bb, "billboarder")
  expect_length(bb$x$bb_opts$customStyle, 5)
 
})
