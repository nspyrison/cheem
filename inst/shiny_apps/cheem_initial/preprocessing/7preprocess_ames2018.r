# FIFA regression, save layer_ls for app -----
## Dependencies ------
require(cheem)
dat  <- amesHousing2018_thin
X    <- dat[, 1:9]
Y    <- log(dat$SalePrice)
clas <- dat$ZoneMS

rf_fit  <- default_rf(X, Y)
shap_df <- attr_df_treeshap(rf_fit, X) ## ~82 sec
this_ls <- cheem_ls(X, Y,
                    model = rf_fit,
                    attr_df = shap_df)
#linked_global_view(this_ls)

## EXPORT OBJECTS ----
if(interactive() == TRUE){
  setwd("~/R/cheem")
  saveRDS(this_ls,
          file = "./inst/shiny_apps/cheem_initial/data/7preprocess_ames2018.rds")
}
if(F) ## Not run, load cheem_ls
  this_ls <- readRDS("./inst/shiny_apps/cheem_initial/data/7preprocess_ames2018.rds")

