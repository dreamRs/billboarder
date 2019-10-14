
context("utils")

test_that("suffix & prefix works", {
  
  expect_is(suffix("%"), "JS_EVAL")
  expect_error(suffix(c("%", "%")))
  
  expect_is(prefix("%"), "JS_EVAL")
  expect_error(prefix(c("%", "%")))
  
})

