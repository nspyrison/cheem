### Preprocessing data for shiny app:
{
  require(cheem)
  require(shapviz)
  require(treeshap)
  require(randomForest)
  require(xgboost)
  require(tictoc)
  require(beepr)
  set.seed(135) ## not sure which models will be impacted, but plausibly helps replication
  #s <- function(sec = .01)Sys.sleep(sec) ## Not in use
  
  tic("Preprocess all")
  fp <- "./inst/preprocessing/"
  wd <- getwd()
  if(substr(wd, nchar(wd) - 6, nchar(wd)) != "R/cheem")
    warning("work directory is not set to cheem package root!")
  if(F) 
    dir("./inst/shiny_apps/cheem/preprocessing")
  
  ## Recover the classification examples
  default_rf <- function(
      x, y, verbose = getOption("verbose"),
      hp_ntree = 125,
      hp_mtry = ifelse(is_discrete(y), sqrt(ncol(x)), ncol(x) / 3),
      hp_nodesize = max(ifelse(is_discrete(y), 1, 5), nrow(x) / 500),
      ...
  ){
    if(verbose) tictoc::tic("default_rf_treeshap")
    .fit <- randomForest::randomForest(
      x, y, mtry = hp_mtry, nodesize = hp_nodesize, ntree = hp_ntree, ...)
    if(verbose) tictoc::toc()
    .fit
  }
  
  
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
