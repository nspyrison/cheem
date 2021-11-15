## Dependencies ------
{
  require(cheem)
  if(F){
    ?amesHousing2018
    str(amesHousing2018)
    hist(as.integer(amesHousing2018$Neighborhd))
    table(amesHousing2018$Neighborhd)
  }
  
  ## Subset to the largest neighborhood; North Ames
  r_idx <- amesHousing2018$Neighborhd == "NAmes"
  dat  <- amesHousing2018_thin[r_idx, ]
  X    <- dat[, 1:9]
  Y    <- log(dat$SalePrice)
  ## class is now zone subclass
  clas <- factor(
    amesHousing2018$SubclassMS[r_idx],
    levels = unique(amesHousing2018$SubclassMS[r_idx]))
}

rf_fit  <- default_rf(X, Y)
shap_df <- attr_df_treeshap(rf_fit, X) ## ~82 sec
this_ls <- cheem_ls(X, Y,
                    model = rf_fit,
                    attr_df = shap_df)
names(this_ls)
global_view(this_ls)
## EXPORT OBJECTS ----
if(interactive() == TRUE){
  setwd("~/R/cheem")
  saveRDS(this_ls,
          file = "./inst/shiny_apps/cheem_initial/data/7preprocess_ames2018.rds")
}
if(F){ ## Not run, load this_ls
  this_ls <- readRDS("./inst/shiny_apps/cheem_initial/data/7preprocess_ames2018.rds")
  
  lapply(this_ls, object.size)
  
  
}