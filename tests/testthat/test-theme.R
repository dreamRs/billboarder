
context("theme")

test_that("set_theme works", {
  
  set_theme("billboard")
  expect_identical(getOption("billboard.theme"), "billboard.min.css")
  
  set_theme("insight")
  expect_identical(getOption("billboard.theme"), "insight.min.css")
  
  set_theme("graph")
  expect_identical(getOption("billboard.theme"), "graph.min.css")
  
})
