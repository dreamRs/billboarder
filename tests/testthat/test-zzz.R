
context("zzz")

test_that("bb_empty works", {
  
  empty <- bb_empty()
  expect_is(empty, "list")
  
  .onLoad()
  expect_is(getOption("bb.empty"), "list")
  
})
