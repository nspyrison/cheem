### Preprocessing data for shiny app:
{
  require(cheem)
  require(shapviz)
  require(treeshap)
  require(randomForest)
  require(xgboost)
  #s <- function(sec = .01)Sys.sleep(sec) ## Not in use
  require(tictoc)
  require(beepr)
  
  tic("Preprocess all")
  wd <- getwd()
  fp <- "./inst/shiny_apps/cheem/preprocessing/"
  if(substr(wd, nchar(wd) - 6, nchar(wd)) != "R/cheem")
    warning("work directory is not set to cheem package root!")
  if(F) 
    dir("./inst/shiny_apps/cheem/preprocessing")
  
  ## Classification:
  source(paste0(fp, "preprocess_toy_classification.r"))
  source(paste0(fp, "preprocess_chocolates.r"))
  source(paste0(fp, "preprocess_penguins.r"))
  ## Regression:
  source(paste0(fp, "preprocess_toy_quad_regression.r"))
  source(paste0(fp, "preprocess_toy_trig_regression.r"))
  source(paste0(fp, "preprocess_toy_mixture_regression.r"))
  source(paste0(fp, "preprocess_ames2018.r"))
  tic("preprocess_fifa")
  source(paste0(fp, "preprocess_fifa.r")) ## Longest by far, 
  toc()
  
  if(F){ ## Open individual files.
    ## Classification
    file.edit(paste0(fp, "preprocess_toy_classification.r"))
    file.edit(paste0(fp, "preprocess_penguins.r"))
    file.edit(paste0(fp, "preprocess_toy_classification.r"))
    ## Regression
    file.edit(paste0(fp, "preprocess_fifa.r"))
    file.edit(paste0(fp, "preprocess_ames2018.r"))
    file.edit(paste0(fp, "preprocess_toy_quad_regression.r"))
    file.edit(paste0(fp, "preprocess_toy_trig_regression.r"))
    file.edit(paste0(fp, "preprocess_chocolates.r"))
  }
  toc()
  beep(4)
}
