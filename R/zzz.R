.onLoad <- function(libname, pkgname) {
  options("bb.empty" = bb_empty())
  invisible()
}

bb_empty <- function() {
  list(
    data = list(
      type = "bar", 
      json = list(value = list(1)),
      filter = JS("function(v) {return v.id !== 'value';}")
    ),
    axis = list(x = list(show = FALSE), y = list(show = FALSE)),
    grid = list(y = list(lines = list(list(
      value = 0.5, text = getOption(x = "bb.empty.text", default = "No data"), 
      position  = "middle", class = "empty"
    )))),
    legend = list(show = FALSE),
    tooltip = list(show = FALSE),
    customstyle = list(
      custom_style = paste(
        ".bb-ygrid-line.empty line{stroke: transparent;}",
        ".bb-ygrid-line.empty text{font-size: 200%;}"
      )
    )
  )
}

