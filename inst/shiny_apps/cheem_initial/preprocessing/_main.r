### Preprocessing data for shiny app:

setwd("~/R/cheem")
if(F)
  dir("./inst/shiny_apps/cheem_initial/preprocessing")
fp <- "./inst/shiny_apps/cheem_initial/preprocessing/"


source(paste0(fp, "1preprocess_penguins.r"))
source(paste0(fp, "2preprocess_toy_classification.r"))
source(paste0(fp, "3preprocess_fifa.r"))
source(paste0(fp, "7preprocess_ames2018.r"))
source(paste0(fp, "8preprocess_toy_regression.r"))