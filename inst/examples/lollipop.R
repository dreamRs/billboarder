

#  ------------------------------------------------------------------------
#
# Title : Lollipop
#    By : VP
#  Date : 2017-08-24
#    
#  ------------------------------------------------------------------------



# Packages ----------------------------------------------------------------

library("billboarder")



# Data --------------------------------------------------------------------

# From wikipedia
sw <- data.frame(
  film = c("The Force Awakens", "The Phantom Menace", "Revenge of the Sith",
           "A New Hope", "Attack of the Clones", "The Empire Strikes Back", "Return of the Jedi"
  ),
  worldwide_gross = c(2068178225, 1027044677, 848754768,
                      775398007, 649398328, 538375067, 475106177)
)



# Lollipop ----------------------------------------------------------------


# raw
lll <- billboarder() %>% 
  bb_barchart(data = cbind(sw, lollipop = sw$worldwide_gross), rotated = TRUE) %>% 
  bb_bar(width = list(ratio = 0.01)) %>% 
  bb_x_axis(tick = list(centered = TRUE)) %>% 
  bb_data(types = list(lollipop = "scatter", worldwide_gross = "bar")) %>% 
  bb_colors_manual(lollipop = "steelblue", worldwide_gross = "#000") %>%
  bb_point(r = 8) %>% 
  bb_add_style(".bb-circle" = "opacity: 1 !important;") %>% 
  bb_legend(hide = TRUE) 

lll


# prettier
lll %>% 
  bb_y_grid(show = TRUE) %>% 
  bb_y_axis(tick = list(
    values = c(0, 5e+08, 1e+09, 1.5e+09, 2e+09),
    outer = FALSE,
    format = htmlwidgets::JS("d3.formatPrefix('$,.0', 1e6)")
  )) %>% 
  bb_labs(
    title = "Star Wars - Total Lifetime Grosses",
    caption = "Data source : wikipedia"
  )


# but tooltip is broke...


