### Preprocessing data for shiny app:
setwd("~/R/cheem")
fp <- "./inst/shiny_apps/cheem_initial/preprocessing/"
if(F)
  dir("./inst/shiny_apps/cheem_initial/preprocessing")

source(paste0(fp, "preprocess_penguins.r"))
source(paste0(fp, "preprocess_toy_classification.r"))
source(paste0(fp, "preprocess_fifa.r"))
source(paste0(fp, "preprocess_ames2018.r"))
source(paste0(fp, "preprocess_toy_quad_regression.r"))
source(paste0(fp, "preprocess_toy_trig_regression.r"))
source(paste0(fp, "preprocess_chocolates.r"))
if(F)
  rm("this_ls")