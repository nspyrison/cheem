## Dependencies ------
## Local files
require("cheem")
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
### SHAP layer_ls -----
layer_ls <- nested_local_attr_layers(
  x = X_wide, y = Y_wide, basis_type = "pca", class = clas_wide)
names(layer_ls)

### EXPORT OBJECTS -----
if(interactive() == TRUE){
  setwd("~/R/cheem")
  save(layer_ls,
       file = "./inst/shiny_apps/cheem_initial/data/6preprocess_diabetes_wide.RData")
}
if(F) ## Not run, load dat, clas, layer_ls
  load("./inst/shiny_apps/cheem_initial/data/6preprocess_diabetes_wide.RData")


## diabetes long: -----
### SHAP layer_ls -----
layer_ls <- nested_local_attr_layers(
  x = X_long, y = Y_long, basis_type = "pca", class = clas_long)
names(layer_ls)

### EXPORT OBJECTS -----
if(interactive() == TRUE){
  setwd("~/R/cheem")
  save(layer_ls,
       file = "./inst/shiny_apps/cheem_initial/data/6preprocess_diabetes_long.RData")
}
if(F) ## Not run, load layer_ls
  load("./inst/shiny_apps/cheem_initial/data/6preprocess_diabetes_long.RData")
