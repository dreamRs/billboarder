
context("billboarder")

test_that("billboarder works", {
  
  bb <- billboarder()
  
  expect_is(bb, "billboarder")
  
})



test_that("billboarder with list works", {
  
  bb <- billboarder(list(
    data = list(
      json = list(
        x = 1:10
      ),
      type = "bar"
    )
  ))
  
  expect_is(bb, "billboarder")
  
})


test_that("billboarder empty (function) works", {
  
  options("bb.empty" = bb_empty)
  bb <- billboarder()
  
  expect_is(bb, "billboarder")
  
})


test_that("billboard_dependency works", {
  
  bbdeps <- billboard_dependencies()
  
  expect_is(bbdeps, "html_dependency")
  
})


test_that("billboard_dependency works", {
  
  bbhtml <- billboarder_html(id = "id", style = "style", class = "class")
  
  expect_is(bbhtml, "shiny.tag")
  
})


