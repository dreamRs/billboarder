

#  ------------------------------------------------------------------------
#
# Title : Avengers Power Grid
#    By : Victor
#  Date : 2018-07-08
#    
#  ------------------------------------------------------------------------


# Source : http://bl.ocks.org/chrisrzhou/2421ac6541b68c1680f8
# and : http://marvel.wikia.com/wiki/Power_Grid


# Packages ----------------------------------------------------------------

library( data.table )




# Data --------------------------------------------------------------------


avengers <- fread(
  input = "http://bl.ocks.org/chrisrzhou/raw/2421ac6541b68c1680f8/e9fe262498c65161f13838d9fd08f87f895a7644/data_the_avengers.csv"
)
avengers


avengers_wide <- dcast(data = avengers, formula = axis ~ group, value.var = "value")


avengers <- as.data.frame(avengers)
avengers_wide <- as.data.frame(avengers_wide)

usethis::use_data(avengers, avengers_wide)








