
context("billboarder-shiny")

test_that("billboarderProxy works", {
  
  session <- as.environment(list(
    ns = identity,
    sendCustomMessage = function(type, message) {
      session$lastCustomMessage <- list(type = type, message = message)
    }
  ))
  
  proxy <- billboarderProxy("mybb", session = session)
  
  expect_is(proxy, "billboarder_Proxy")
  
})


test_that("billboarder-shiny works", {
  
  server <- renderBillboarder(billboarder())
  ui <- billboarderOutput(outputId = "bb")

  expect_is(ui, "shiny.tag.list")
  expect_true(length(htmltools::findDependencies(ui)) >= 2)
  expect_is(server, "function")
  
})



test_that("do not use with billboarder()", {
  
  session <- as.environment(list(
    ns = identity,
    sendCustomMessage = function(type, message) {
      session$lastCustomMessage <- list(type = type, message = message)
    }
  ))
  
  expect_error(billboarder() %>% bb_proxy_axis_labels())
  expect_error(billboarder() %>% bb_proxy_xs("a"))
  
})





test_that("bb_load works", {
  
  session <- as.environment(list(
    ns = identity,
    sendCustomMessage = function(type, message) {
      session$lastCustomMessage <- list(type = type, message = message)
    }
  ))
  
  dat <- data.frame(
    labels = c("A", "B", "C"), 
    values = c(1, 2, 3),
    groups = c("a", "a", "b")
  )
  
  proxy <- billboarderProxy("mybb", session = session) %>% 
    bb_load(data = dat)
  
  expect_is(proxy, "billboarder_Proxy")
  expect_identical(session$lastCustomMessage$type, "update-billboard-load")

})


test_that("bb_unload works", {
  
  session <- as.environment(list(
    ns = identity,
    sendCustomMessage = function(type, message) {
      session$lastCustomMessage <- list(type = type, message = message)
    }
  ))

  
  proxy <- billboarderProxy("mybb", session = session) %>% 
    bb_unload()
  
  expect_is(proxy, "billboarder_Proxy")
  expect_true(proxy$unload)
  
  
  proxy <- billboarderProxy("mybb", session = session) %>% 
    bb_unload(ids = "A")
  
  expect_is(proxy, "billboarder_Proxy")
  expect_identical(proxy$unload, "A")
  
})



test_that("bb_proxy_focus works", {
  
  session <- as.environment(list(
    ns = identity,
    sendCustomMessage = function(type, message) {
      session$lastCustomMessage <- list(type = type, message = message)
    }
  ))
  
  proxy <- billboarderProxy("mybb", session = session) %>% 
    bb_proxy_focus()
  
  expect_is(proxy, "billboarder_Proxy")
  expect_identical(session$lastCustomMessage$type, "update-billboard-focus")
})



test_that("bb_proxy_defocus works", {
  
  session <- as.environment(list(
    ns = identity,
    sendCustomMessage = function(type, message) {
      session$lastCustomMessage <- list(type = type, message = message)
    }
  ))
  
  proxy <- billboarderProxy("mybb", session = session) %>% 
    bb_proxy_defocus()
  
  expect_is(proxy, "billboarder_Proxy")
  expect_identical(session$lastCustomMessage$type, "update-billboard-defocus")
})



test_that("bb_proxy_axis_labels works", {
  
  session <- as.environment(list(
    ns = identity,
    sendCustomMessage = function(type, message) {
      session$lastCustomMessage <- list(type = type, message = message)
    }
  ))
  
  proxy <- billboarderProxy("mybb", session = session) %>% 
    bb_proxy_axis_labels(x = "xlab", y = "ylab")
  
  expect_is(proxy, "billboarder_Proxy")
  expect_identical(session$lastCustomMessage$type, "update-billboard-axis_labels")
})



test_that("bb_proxy_xs works", {
  
  session <- as.environment(list(
    ns = identity,
    sendCustomMessage = function(type, message) {
      session$lastCustomMessage <- list(type = type, message = message)
    }
  ))
  
  proxy <- billboarderProxy("mybb", session = session) %>% 
    bb_proxy_xs(xs = letters)
  
  expect_is(proxy, "billboarder_Proxy")
  expect_identical(session$lastCustomMessage$type, "update-billboard-xs")
})




test_that("bb_proxy_groups works", {
  
  session <- as.environment(list(
    ns = identity,
    sendCustomMessage = function(type, message) {
      session$lastCustomMessage <- list(type = type, message = message)
    }
  ))
  
  proxy <- billboarderProxy("mybb", session = session) %>% 
    bb_proxy_groups("x")
  
  expect_is(proxy, "billboarder_Proxy")
  expect_identical(session$lastCustomMessage$type, "update-billboard-groups")
})



test_that("bb_proxy_hide works", {
  
  session <- as.environment(list(
    ns = identity,
    sendCustomMessage = function(type, message) {
      session$lastCustomMessage <- list(type = type, message = message)
    }
  ))
  
  proxy <- billboarderProxy("mybb", session = session) %>% 
    bb_proxy_hide(targetIdsValue = "x")
  
  expect_is(proxy, "billboarder_Proxy")
  expect_identical(session$lastCustomMessage$type, "update-billboard-hide")
})


test_that("bb_proxy_show works", {
  
  session <- as.environment(list(
    ns = identity,
    sendCustomMessage = function(type, message) {
      session$lastCustomMessage <- list(type = type, message = message)
    }
  ))
  
  proxy <- billboarderProxy("mybb", session = session) %>% 
    bb_proxy_show(targetIdsValue = "x")
  
  expect_is(proxy, "billboarder_Proxy")
  expect_identical(session$lastCustomMessage$type, "update-billboard-show")
})



test_that("bb_proxy_legend works", {
  
  session <- as.environment(list(
    ns = identity,
    sendCustomMessage = function(type, message) {
      session$lastCustomMessage <- list(type = type, message = message)
    }
  ))
  
  proxy <- billboarderProxy("mybb", session = session) %>% 
    bb_proxy_legend(targetIds = "x")
  
  expect_is(proxy, "billboarder_Proxy")
  expect_identical(session$lastCustomMessage$type, "update-billboard-legend-show")
  
  proxy <- billboarderProxy("mybb", session = session) %>% 
    bb_proxy_legend(targetIds = "x", what = "hide")
  
  expect_is(proxy, "billboarder_Proxy")
  expect_identical(session$lastCustomMessage$type, "update-billboard-legend-hide")
})



test_that("bb_proxy_tooltip works", {
  
  session <- as.environment(list(
    ns = identity,
    sendCustomMessage = function(type, message) {
      session$lastCustomMessage <- list(type = type, message = message)
    }
  ))
  
  proxy <- billboarderProxy("mybb", session = session) %>% 
    bb_proxy_tooltip(x = 5)
  
  expect_is(proxy, "billboarder_Proxy")
  expect_identical(session$lastCustomMessage$type, "update-billboard-tooltip-show")
  
  proxy <- billboarderProxy("mybb", session = session) %>% 
    bb_proxy_tooltip(x = 5, what = "hide")
  
  expect_is(proxy, "billboarder_Proxy")
  expect_identical(session$lastCustomMessage$type, "update-billboard-tooltip-hide")
})




test_that("bb_proxy_data_names works", {
  
  session <- as.environment(list(
    ns = identity,
    sendCustomMessage = function(type, message) {
      session$lastCustomMessage <- list(type = type, message = message)
    }
  ))
  
  proxy <- billboarderProxy("mybb", session = session) %>% 
    bb_proxy_data_names(old = "x", new = "a")
  
  expect_is(proxy, "billboarder_Proxy")
  expect_identical(session$lastCustomMessage$type, "update-billboard-data-names")
})




test_that("bb_proxy_data_colors works", {
  
  session <- as.environment(list(
    ns = identity,
    sendCustomMessage = function(type, message) {
      session$lastCustomMessage <- list(type = type, message = message)
    }
  ))
  
  proxy <- billboarderProxy("mybb", session = session) %>% 
    bb_proxy_data_colors(names = "x", colors = "firebrick")
  
  expect_is(proxy, "billboarder_Proxy")
  expect_identical(session$lastCustomMessage$type, "update-billboard-data-colors")
})



test_that("bb_proxy_flow works", {
  
  session <- as.environment(list(
    ns = identity,
    sendCustomMessage = function(type, message) {
      session$lastCustomMessage <- list(type = type, message = message)
    }
  ))
  
  proxy <- billboarderProxy("mybb", session = session) %>% 
    bb_proxy_flow(json = list(
      x = list(format(Sys.time())),
      y = list(round(rnorm(1) * 10))
    ), data = list(x = "x"))
  
  expect_is(proxy, "billboarder_Proxy")
  expect_identical(session$lastCustomMessage$type, "update-billboard-flow")
})



