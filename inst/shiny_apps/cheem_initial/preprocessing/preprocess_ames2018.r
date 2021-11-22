## Dependencies ------
{
  require(cheem)
  s <- function(sec = .01)Sys.sleep(sec)
  if(F)
    str(amesHousing2018_NorthAmes)
  dat  <- amesHousing2018_NorthAmes
  X    <- dat[, 1:9]
  Y    <- log(dat$SalePrice)
  ## class is now zone subclass
  clas <- amesHousing2018_NorthAmes$SubclassMS
}

rf_fit  <- default_rf(X, Y); s();
shap_df <- attr_df_treeshap(rf_fit, X); s(); ## ~82 sec
this_ls <- cheem_ls(X, Y, class = clas,
                    model = rf_fit,
                    attr_df = shap_df)
if(F){
  names(this_ls)
  global_view(this_ls)
}


## EXPORT OBJECTS ----
setwd("~/R/cheem")
saveRDS(this_ls,
        file = "./inst/shiny_apps/cheem_initial/data/preprocess_ames2018.rds")
cat("Saved.\n")
if(F){ ## Not run, load this_ls
  this_ls <- readRDS("./inst/shiny_apps/cheem_initial/data/preprocess_ames2018.rds")
  
  lapply(this_ls, object.size)
  
  
}