### Preprocessing data for shiny app:
setwd("~/R/cheem")
fp <- "./inst/shiny_apps/cheem_initial/preprocessing/"
if(F)
  dir("./inst/shiny_apps/cheem_initial/preprocessing")

source(paste0(fp, "1preprocess_penguins.r"))
source(paste0(fp, "2preprocess_toy_classification.r"))
source(paste0(fp, "3preprocess_fifa.r"))
source(paste0(fp, "7preprocess_ames2018.r"))
source(paste0(fp, "8preprocess_toy_regression.r"))
source(paste0(fp, "9preprocess_toy_regression_trig.r"))
source(paste0(fp, "preprocess_chocolates.r"))
if(F)
  rm("this_ls")