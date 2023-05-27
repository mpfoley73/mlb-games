##
## Aggregate event files from Retrosheet
## https://www.retrosheet.org/game.htm
##

library(tidyverse)
library(baseballr)

wd <- getwd()
get_retrosheet_data(
  path_to_directory = file.path(wd, "data/retrosheet/event"), 
  years_to_acquire = 1914:2017
)
