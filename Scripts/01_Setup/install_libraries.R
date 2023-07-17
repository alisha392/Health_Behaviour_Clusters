# Setup Environment -------------------------------------------------------

# installing loading essential packages - only needs to run once
# !!!!IMPORTANT -gridExtra and grid package used in scripts for deriving descriptives!!!!

install.packages("pacman")
require(pacman)
pacman::p_load(
  tidyverse, ggplot2, dplyr, stringr, here, rio, grid, gridExtra, viridis, gt,
  reshape2, scales, cowplot, ggnewscale, glue, haven, rhdf5, janitor, gt,
  poLCA, gtable, lemon, stringi, kableExtra, MplusAutomation, prettycode,rlang, ggh4x, styler,broom,knitr
)

options(max.print = 100000)


