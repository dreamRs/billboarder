
context("bb_helpers")

test_that("bb_barchart works", {
  
  dat <- data.frame(
    labels = c("A", "B", "C"), 
    values = c(1, 2, 3),
    groups = c("a", "a", "b")
  )
  
  bb <- billboarder(data = dat) %>% 
    bb_barchart(mapping = bbaes(labels, values))
  
  expect_is(bb, "billboarder")
  expect_identical(bb$x$bb_opts$data$type, "bar")
  
  bb <- billboarder(data = dat) %>% 
    bb_barchart(mapping = bbaes(labels, values, fill = groups))
  
  expect_is(bb, "billboarder")
  expect_identical(bb$x$bb_opts$data$type, "bar")
  
})




test_that("bb_bar_color_manual works", {
  
  dat <- data.frame(
    labels = c("A", "B", "C"), 
    values = c(1, 2, 3),
    groups = c("a", "a", "b")
  )
  
  bb <- billboarder(data = dat) %>% 
    bb_barchart(mapping = bbaes(labels, values)) %>% 
    bb_bar_color_manual(values = list(
      A = "firebrick",
      B = "forestgreen",
      C = "steelblue"
    ))
  
  expect_is(bb, "billboarder")
  expect_is(bb$x$bb_opts$data$color, "JS_EVAL")
  
})



test_that("bb_categories works", {
  
  dat <- data.frame(
    labels = c("A", "B", "C"), 
    values = c(1, 2, 3),
    groups = c("a", "a", "b")
  )
  
  bb <- billboarder() %>% 
    bb_categories(categories = c("A", "B"))
  
  expect_is(bb, "billboarder")
  expect_identical(bb$x$bb_opts$axis$x$type, "category")
  expect_identical(bb$x$bb_opts$axis$x$categories, c("A", "B"))
  
})



test_that("bb_scatterplot works", {
  
  dat <- data.frame(
    x = 1:10,
    y = 1:10,
    z = 1:10,
    groups = rep(c("A", "B"), each = 5)
  )
  
  bb <- billboarder(data = dat) %>% 
    bb_scatterplot(mapping = bbaes(x, y))
  
  expect_is(bb, "billboarder")
  expect_identical(bb$x$bb_opts$data$type, "scatter")
  
  bb <- billboarder(data = dat) %>% 
    bb_scatterplot(mapping = bbaes(x, y, size = z))
  
  expect_is(bb, "billboarder")
  expect_identical(bb$x$bb_opts$data$type, "bubble")
  
  bb <- billboarder(data = dat) %>% 
    bb_scatterplot(mapping = bbaes(x, y, group = groups))
  
  expect_is(bb, "billboarder")
  expect_identical(bb$x$bb_opts$data$type, "scatter")
  
  
  bb <- billboarder(data = dat) %>% 
    bb_scatterplot(mapping = bbaes(x, y, group = groups, size = z))
  
  expect_is(bb, "billboarder")
  expect_identical(bb$x$bb_opts$data$type, "bubble")
  
})



test_that("bb_gaugechart works", {

  bb <- billboarder() %>% 
    bb_gaugechart(value = 50)
  
  expect_is(bb, "billboarder")
  expect_identical(bb$x$bb_opts$data$type, "gauge")
  
})





test_that("bb_piechart works", {
  
  dat <- data.frame(
    labels = c("A", "B", "C"), 
    values = c(1, 2, 3),
    groups = c("a", "a", "b")
  )
  
  bb <- billboarder(data = dat) %>% 
    bb_piechart(mapping = bbaes(labels, values))
  
  expect_is(bb, "billboarder")
  expect_identical(bb$x$bb_opts$data$type, "pie")
  
})



test_that("bb_donutchart works", {
  
  dat <- data.frame(
    labels = c("A", "B", "C"), 
    values = c(1, 2, 3),
    groups = c("a", "a", "b")
  )
  
  bb <- billboarder(data = dat) %>% 
    bb_donutchart(mapping = bbaes(labels, values))
  
  expect_is(bb, "billboarder")
  expect_identical(bb$x$bb_opts$data$type, "donut")
  
  bb <- billboarder() %>% 
    bb_donutchart(data = dat)
  
  expect_is(bb, "billboarder")
  expect_identical(bb$x$bb_opts$data$type, "donut")
  
})




test_that("bb_linechart works", {
  
  dat <- data.frame(
    index = seq_len(100),
    Sine = sin(1:100/10),
    Cosine = 0.5 * cos(1:100/10),
    Sine2 = sin(1:100/10) * 0.25 + 0.5
  )
  
  bb <- billboarder(data = dat) %>% 
    bb_linechart(mapping = bbaes(index, Sine), dasharray = 2, width = 5)
  
  expect_is(bb, "billboarder")
  expect_is(bb$x$bb_opts$data$type, "list")
  expect_true(length(bb$x$bb_opts$customStyle) > 0)
  
  
  bb <- billboarder() %>% 
    bb_linechart(data = rnorm(30))
  
  expect_is(bb, "billboarder")
  expect_identical(bb$x$bb_opts$data$type[[1]], "line")
})




test_that("bb_densityplot works", {
  
  dat <- data.frame(
    x = rnorm(100),
    groups = rep(c("A", "B"), each = 50)
  )
  
  bb <- billboarder(data = dat) %>% 
    bb_densityplot(mapping = bbaes(x))
  
  expect_is(bb, "billboarder")
  expect_identical(bb$x$bb_opts$data$type, "area-spline")
  
  bb <- billboarder(data = dat) %>% 
    bb_densityplot(mapping = bbaes(x, group = groups))
  
  expect_is(bb, "billboarder")
  expect_identical(bb$x$bb_opts$data$type, "area-spline")
})




test_that("bb_histogram works", {
  
  dat <- data.frame(
    x = rnorm(100),
    groups = rep(c("A", "B"), each = 50)
  )
  
  bb <- billboarder(data = dat) %>% 
    bb_histogram(mapping = bbaes(x))
  
  expect_is(bb, "billboarder")
  expect_identical(bb$x$bb_opts$data$type, "area-step")
  
  bb <- billboarder(data = dat) %>% 
    bb_histogram(mapping = bbaes(x, group = groups))
  
  expect_is(bb, "billboarder")
  expect_identical(bb$x$bb_opts$data$type, "area-step")
})




test_that("bb_lollipop works", {
  
  dat <- data.frame(
    labels = c("A", "B", "C"), 
    values = c(1, 2, 3),
    groups = c("a", "a", "b")
  )
  
  bb <- billboarder(data = dat) %>% 
    bb_lollipop(mapping = bbaes(labels, values))
  
  expect_is(bb, "billboarder")
  expect_identical(bb$x$bb_opts$data$type, "bar")
  
})





test_that("bb_radarchart works", {
  
  data("avengers_wide")

  bb <- billboarder() %>%
    bb_radarchart(data = avengers_wide)
  
  expect_is(bb, "billboarder")
  expect_identical(bb$x$bb_opts$data$type, "radar")
  
  data("avengers")
  
  bb <- billboarder() %>%
    bb_radarchart(
      data = avengers, 
      mapping = bbaes(x = axis, y = value, group = group)
    )
  
  expect_is(bb, "billboarder")
  expect_identical(bb$x$bb_opts$data$type, "radar")
  
})




