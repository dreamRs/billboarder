
context("bb_utils")

test_that("bb_utils works", {
  
  bb_utils <- c(
    "bb_area", "bb_axis", "bb_bar", "bb_bubble", "bb_color", 
    "bb_colors_manual", "bb_data", "bb_donut", "bb_export", "bb_gauge", 
    "bb_grid", "bb_interaction", "bb_legend", "bb_line", "bb_pie", 
    "bb_point", "bb_radar", "bb_regions", "bb_spline", "bb_subchart", 
    "bb_svg", "bb_title", "bb_tooltip", "bb_transition", "bb_x_axis", 
    "bb_x_grid", "bb_y_axis", "bb_y_grid", "bb_zoom"
  )
  
  for (bb_u in bb_utils) {
    fun <- match.fun(FUN = bb_u)
    bb <- billboarder() %>% 
      fun(arg1 = 1)
    expect_is(bb, "billboarder")
  }
  
})

