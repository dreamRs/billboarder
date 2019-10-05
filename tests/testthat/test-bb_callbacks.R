
context("bb_callbacks")

test_that("bb_callbacks works", {
  
  callbacks <- bb_callbacks(
    bb = billboarder(), 
    onafterinit = "function() {}", 
    onrendered = JS("function() {}")
  )
  expect_is(callbacks, "billboarder")
  
  expect_is(callbacks$x$bb_opts$onafterinit, "JS_EVAL")
  expect_is(callbacks$x$bb_opts$onrendered, "JS_EVAL")
  
  expect_error(bb_callbacks(
    bb = list(), 
    onafterinit = "function() {}"
  ))
  
})
