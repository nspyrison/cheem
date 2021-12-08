### Preprocessing data for shiny app:
setwd("~/R/cheem")
fp <- "./inst/shiny_apps/cheem_initial/preprocessing/"
if(F)
  dir("./inst/shiny_apps/cheem_initial/preprocessing")

## Classification:
source(paste0(fp, "preprocess_toy_classification.r"))
source(paste0(fp, "preprocess_chocolates.r"))
source(paste0(fp, "preprocess_penguins.r"))
## Regression:
source(paste0(fp, "preprocess_toy_quad_regression.r"))
source(paste0(fp, "preprocess_toy_trig_regression.r"))
source(paste0(fp, "preprocess_toy_mixture_regression.r"))
source(paste0(fp, "preprocess_fifa.r"))
source(paste0(fp, "preprocess_ames2018.r"))



if(F)
  rm("this_ls")

if(F){
  file.edit(paste0(fp, "preprocess_penguins.r"))
  file.edit(paste0(fp, "preprocess_toy_classification.r"))
  file.edit(paste0(fp, "preprocess_fifa.r"))
  file.edit(paste0(fp, "preprocess_ames2018.r"))
  file.edit(paste0(fp, "preprocess_toy_quad_regression.r"))
  file.edit(paste0(fp, "preprocess_toy_trig_regression.r"))
  file.edit(paste0(fp, "preprocess_chocolates.r"))
}