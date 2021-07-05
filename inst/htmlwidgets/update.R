

library(fs)
update_asset <- function(x, url = "https://naver.github.io/billboard.js/release/latest/dist") {
  # https://naver.github.io/billboard.js/release/latest/dist/theme/datalab.min.css
  tmp <- tempfile()
  download.file(
    url = file.path(url, x),
    destfile = tmp
  )
  file_copy(
    path = tmp,
    new_path = file.path("inst/htmlwidgets/lib/billboard", x),
    overwrite = TRUE
  )
  unlink(tmp)
}

update_asset("billboard.pkgd.min.js")
update_asset("billboard.min.css")
update_asset("datalab.min.css", "https://naver.github.io/billboard.js/release/latest/dist/theme")
update_asset("graph.min.css", "https://naver.github.io/billboard.js/release/latest/dist/theme")
update_asset("insight.min.css", "https://naver.github.io/billboard.js/release/latest/dist/theme")
