
context("proxy_example")

test_that("proxy_example works", {
  
  default <- proxy_example()
  expect_is(default, "shiny.appobj")
  
  examples <- c("gauge", "pie", "bar", "bar2", "line", "line2",
                "density", "histogram", "stacked_bar", "lollipop")
  
  for (i in examples) {
    ex <- proxy_example(i)
    expect_is(ex, "shiny.appobj")
  }
  
})
