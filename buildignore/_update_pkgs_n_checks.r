pkgs <- c("ggplot2", "plotly", "magrittr", "shiny", "shinythemes", 
          "shinycssloaders", "DT", "treeshap", "randomForest", "tourr", 
          "lqmm", "mvtnorm", "gganimate", "dplyr", "tidyr", "tictoc", "beepr", 
          "knitr", "testthat", "spelling", "rmarkdown")
sapply(pkgs, install.packages)

if(F){
  devtools::document()
  devtools::build_vignettes()
  beepr::beep()
  devtools::build_site()
  beepr::beep()
  devtools::check()
  beepr::beep()
  nsCheckRhub()
}