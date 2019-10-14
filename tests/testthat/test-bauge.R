
context("bauge")

test_that("bauge works", {
  
  b <- bauge(value = 45, min = 0, max = 100, colors = "#123456")
  
  expect_is(b, "bauge")
  expect_is(b$x, "list")
  expect_identical(b$x$data$json$value[[1]], expected = 45)
})


test_that("bauge-shiny works", {
  
  server <- renderBauge(bauge())
  ui <- baugeOutput(outputId = "bb")
  
  expect_is(ui, "shiny.tag.list")
  expect_true(length(htmltools::findDependencies(ui)) >= 2)
  expect_is(server, "function")
  
})
