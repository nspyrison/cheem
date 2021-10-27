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
### cheem_ls -----
cheem_ls <- cheem_ls(
  x = X_wide, y = Y_wide, basis_type = "pca", class = clas_wide)
names(cheem_ls)

### EXPORT OBJECTS -----
if(interactive() == TRUE){
  setwd("~/R/cheem")
  saveRDS(cheem_ls,
       file = "./inst/shiny_apps/cheem_initial/data/6preprocess_diabetes_wide.rds")
}
if(F) ## Not run, load cheem_ls
  cheem_ls <- readRDS("./inst/shiny_apps/cheem_initial/data/6preprocess_diabetes_wide.rds")


## diabetes long: -----
### cheem_ls -----
cheem_ls <- cheem_ls(
  x = X_long, y = Y_long, basis_type = "pca", class = clas_long)
names(cheem_ls)

### EXPORT OBJECTS -----
if(interactive() == TRUE){
  setwd("~/R/cheem")
  saveRDS(cheem_ls,
          file = "./inst/shiny_apps/cheem_initial/data/6preprocess_diabetes_long.rds")
}
if(F) ## Not run, load cheem_ls
  cheem_ls <- readRDS("./inst/shiny_apps/cheem_initial/data/6preprocess_diabetes_long.rds")
