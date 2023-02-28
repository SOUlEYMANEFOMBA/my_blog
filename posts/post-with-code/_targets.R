library(targets)
"script"
source("R/fonctions.R")
tar_option_set(packages = c("heron","quarto","targets"))

list(tar_target(dicide_triangle, divide_triangle(0,0,0,1,0.5,sqrt(3/2))),
  tar_target(list_triangle,divide_list_triangle(divide_triangle(0,0,0,1,0.5,sqrt(3/2)))),
  tar_target(plot, plot_triangle(0,0,0,1,0.5,sqrt(3/2)))
)