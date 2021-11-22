## Dependencies ------
## Local files
require(cheem)
{
  dat_wide <- spinifex::PimaIndiansDiabetes_wide
  X_wide <- dat_wide[, -9]
  Y_wide <- as.integer(dat_wide[, 9])
  clas_wide <- dat_wide[, 9]
  dat_long <- spinifex::PimaIndiansDiabetes_long
  X_long <- dat_long[, -7]
  Y_long <- as.integer(dat_long[, 7])
  clas_long <- dat_long[, 7]
}

## diabetes wide: -----
rf_fit  <- default_rf(X_wide, Y_wide)
shap_df <- attr_df_treeshap(rf_fit, X_wide)
this_ls <- cheem_ls(X_wide, Y_wide, class = clas_wide,
                    model = rf_fit,
                    attr_df = shap_df)

### EXPORT OBJECTS -----
setwd("~/R/cheem")
saveRDS(this_ls,
        file = "./inst/shiny_apps/cheem_initial/data/preprocess_diabetes_wide.rds")
if(F) ## Not run, load this_ls
  this_ls <- readRDS("./inst/shiny_apps/cheem_initial/data/preprocess_diabetes_wide.rds")


## diabetes long: -----
rf_fit  <- default_rf(X_long, Y_long)
shap_df <- attr_df_treeshap(rf_fit, X_long)
this_ls <- cheem_ls(X_long, Y_long, class = clas_long,
                    model = rf_fit,
                    attr_df = shap_df)

### EXPORT OBJECTS -----
setwd("~/R/cheem")
saveRDS(this_ls,
        file = "./inst/shiny_apps/cheem_initial/data/preprocess_diabetes_long.rds")
if(F) ## Not run, load this_ls
  this_ls <- readRDS("./inst/shiny_apps/cheem_initial/data/preprocess_diabetes_long.rds")
