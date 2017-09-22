

# Synchronized charts -----------------------------------------------------

# https://groups.google.com/forum/#!searchin/c3js/synchronize%7Csort:relevance/c3js/XQiiJnWlfzc/t2tqOapVAQAJ



library("billboarder")
library("htmltools")



bbcos <- billboarder() %>% 
  bb_linechart(data = cos(seq(-pi, pi, length.out = 50)))
bbcos

bbsin <- billboarder() %>% 
  bb_linechart(data = sin(seq(-pi, pi, length.out = 50)))
bbsin


multibb <- tags$table(
  tags$tr(
    width="100%", style="width:100%",
    tags$th(
      bbcos,
      bbsin
    )
  )
)

htmltools::html_print(multibb)


bb_gridlayout(bbcos, bbsin)
htmltools::html_print(bb_gridlayout(bbcos, bbsin))



bb_gridlayout <- function(...) {
  
  input_list <- as.list(substitute(list(...)))[-1L]
  
  params <- list()
  
  for (i in 1:length(input_list)) {
    x <- eval.parent(input_list[[i]])
    if (any(class(x) == "list")) {
      for (j in 1:length(x)) {
        y <- eval(x[[j]])
        params[[length(params)+1]] <- y
      }
    } else {
      params[[length(params)+1]] <- x
    }
  }
  
  multibb <- tags$table(
    tags$tr(
      width="100%", style="width:100%",
      tags$th(
        params
      )
    )
  )
  return(multibb)
}









