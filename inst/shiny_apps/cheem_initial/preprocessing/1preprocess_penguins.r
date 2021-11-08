## Dependencies ------
require(cheem)

## Data preprocess ------
## Data setup, spinifex::penguins
{
  raw <- spinifex::penguins
  clas <- raw$species
  lvls <- levels(clas)
  # ## Filter to closest 2 classes
  # raw <- raw[raw$species %in% lvls[1:2], ]
  # clas <- factor(raw$species, levels = lvls[1:2]) ## Manually remove 3rd lvl
  X <- raw[, 1:4] %>% as.data.frame() ## X's not scaled.
  colnames(X) <- c("b_l", "b_d", "f_l", "wgt")
  Y <- as.integer(raw$species)
}

## cheem_ls -----
cheem_ls <- cheem_ls(
  x = X, y = Y, basis_type = "pca", class = clas)
names(cheem_ls)
cheem_ls$basis_ls


## EXPORT OBJECTS ----
if(interactive() == TRUE){
  setwd("~/R/cheem")
  saveRDS(cheem_ls,
          file = "./inst/shiny_apps/cheem_initial/data/1preprocess_penguins.rds")
}
if(F) ## Not run, load cheem_ls
  cheem_ls <- readRDS("./inst/shiny_apps/cheem_initial/data/1preprocess_penguins.rds")

