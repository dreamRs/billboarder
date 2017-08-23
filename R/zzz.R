.onLoad <- function(libname, pkgname) {
  
  options(
    "bb.empty" = function() {
      list(
        data = list(json = list()),
        axis = list(x = list(show = FALSE), y = list(show = FALSE)),
        grid = list(y = list(lines = list(list(
          value = 0.5, text = getOption(x = "bb.empty.text", default = "No data"), 
          position  = "middle", class = "empty"
        )))),
        customstyle = list(
          custom_style = paste(
            ".bb-ygrid-line.empty line{stroke: transparent;}",
            ".bb-ygrid-line.empty text{font-size: 200%;}"
          )
        )
      )
    }
  )
  
  invisible()
}
