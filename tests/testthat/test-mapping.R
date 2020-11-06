
context("Mapping")


# bbaes -------------------------------------------------------------------


test_that("bbaes - one var", {
  
  aes <- bbaes(x = Sepal.Length)
  
  expect_length(object = aes, n = 1)
  expect_is(aes$x, "quosure")

})

test_that("bbaes - two var", {
  
  aes <- bbaes(x = Sepal.Length, y = Sepal.Width)
  
  expect_length(object = aes, n = 2)
  expect_is(aes$x, "quosure")
  expect_is(aes$y, "quosure")

})

test_that("bbaes - group var", {
  
  aes <- bbaes(x = Sepal.Length, y = Sepal.Width, group = Species)
  
  expect_length(object = aes, n = 3)
  expect_is(aes$x, "quosure")
  expect_is(aes$y, "quosure")
  expect_is(aes$group, "quosure")
  
})



# bb_aes ------------------------------------------------------------------


test_that("bbaes - one var", {
  
  aes <- bb_aes(list(), Sepal.Length)
  
  expect_false(is.null(aes$x$mapping))
  expect_length(object = aes$x$mapping, n = 1)
  expect_is(aes$x$mapping$x, "quosure")
  
})

test_that("bbaes - two var", {
  
  aes <- bb_aes(list(), x = Sepal.Length, y = Sepal.Width)
  
  expect_false(is.null(aes$x$mapping))
  expect_length(object = aes$x$mapping, n = 2)
  expect_is(aes$x$mapping$x, "quosure")
  expect_is(aes$x$mapping$y, "quosure")
  
})

test_that("bbaes - group var", {
  
  aes <- bb_aes(list(), x = Sepal.Length, y = Sepal.Width, group = Species)
  
  expect_false(is.null(aes$x$mapping))
  expect_length(object = aes$x$mapping, n = 3)
  expect_is(aes$x$mapping$x, "quosure")
  expect_is(aes$x$mapping$y, "quosure")
  expect_is(aes$x$mapping$group, "quosure")
  
})




# bbaes_string ------------------------------------------------------------

test_that("bbaes_string - one var", {
  
  aes <- bbaes_string(x = "Sepal.Length")
  
  expect_equal(object = aes, expected = bbaes(x = Sepal.Length))
  
})

test_that("bbaes_string - two var", {
  
  aes <- bbaes_string(x = "Sepal.Length", y = "Sepal.Width")
  
  expect_equal(object = aes, expected = bbaes(x = Sepal.Length, y = Sepal.Width))
  
})




# bb_aes_string -----------------------------------------------------------

test_that("bb_aes_string - one var", {
  
  aes <- bb_aes_string(list(), x = "Sepal.Length")
  
  expect_equal(object = aes, expected = bb_aes(list(), x = Sepal.Length))
  
})

test_that("bb_aes_string - two var", {
  
  aes <- bb_aes_string(list(), x = "Sepal.Length", y = "Sepal.Width")
  
  expect_equal(object = aes, expected = bb_aes(list(), x = Sepal.Length, y = Sepal.Width))
  
})





# bbmapping ---------------------------------------------------------------


test_that("bbmapping - one var", {
  
  mapping <- bbmapping(iris, bbaes(x = Sepal.Length))
  
  expect_length(object = mapping, n = 1)
  expect_named(object = mapping, expected = "Sepal.Length")
  expect_identical(object = mapping$Sepal.Length, expected = iris$Sepal.Length)
})


test_that("bbmapping - two var", {
  
  mapping <- bbmapping(iris, bbaes(x = Sepal.Length, y = Sepal.Width))
  
  expect_length(object = mapping, n = 2)
  expect_named(object = mapping, expected = c("Sepal.Length", "Sepal.Width"))
  expect_identical(object = mapping$Sepal.Length, expected = iris$Sepal.Length)
  expect_identical(object = mapping$Sepal.Width, expected = iris$Sepal.Width)
})


test_that("bbmapping - group var", {
  
  tab <- table(sample(letters[1:5], 100, TRUE), sample(LETTERS[1:5], 100, TRUE))
  tab <- as.data.frame(tab)
  mapping <- bbmapping(tab, bbaes(x = Var1, y = Freq, group = Var2))
  
  expect_length(object = mapping, n = 6)
  expect_length(object = mapping$Var1, n = 5)
  expect_named(object = mapping, expected = c("Var1", LETTERS[1:5]))
  expect_identical(object = mapping$A, expected = tab$Freq[tab$Var2 == "A"])
  
  # With duplicated x
  mapping <- bbmapping(tab, bbaes(x = Var1, y = Freq))
  expect_length(object = mapping, n = 2)
  expect_length(object = mapping$Var1, n = 5)
})


test_that("bbmapping - ymin & ymax", {
  dat <- data.frame(
    date = seq.Date(Sys.Date(), length.out = 20, by = "day"),
    y1 = round(rnorm(20, 100, 15)),
    y2 = round(rnorm(20, 100, 15)),
    group = rep(c("A", "B"), each = 10)
  )
  dat$ymin1 <- dat$y1 - 5
  dat$ymax1 <- dat$y1 + 5
  
  dat$ymin2 <- dat$y2 - sample(3:15, 20, TRUE)
  dat$ymax2 <- dat$y2 + sample(3:15, 20, TRUE)
  
  
  mapping <- bbmapping(dat, bbaes(x = date, y = y1, ymin = ymin1, ymax = ymax1))
  expect_length(object = mapping, n = 2)
  expect_length(object = mapping$y1, n = 20)
  expect_length(object = mapping$y1[[1]], n = 3)
  
  
  mapping <- bbmapping(dat, bbaes(x = date, y = y1, ymin = ymin1, ymax = ymax1, group = group))
  expect_length(object = mapping, n = 3)
  expect_true(all(c("A", "B") %in% names(mapping)))
})









