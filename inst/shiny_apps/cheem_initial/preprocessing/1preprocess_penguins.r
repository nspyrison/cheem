## Dependencies ------
require("cheem")

## Data preprocess ------
## Data setup, spinifex::penguins
{
  raw <- spinifex::penguins
  lvls <- levels(raw$species)
  ## Filter to closest 2 classes
  raw <- raw[raw$species %in% lvls[1:2], ]
  X <- raw[, 1:4] %>% as.data.frame() ## X's not scaled.
  colnames(X) <- c("b_l", "b_d", "f_l", "wgt")
  Y <- as.integer(raw$species)
  clas <- factor(raw$species, levels = lvls[1:2]) ## Manually remove 3rd lvl
}

## SHAP layer_ls -----
layer_ls <- nested_local_attr_layers(
  x = X, y = Y, basis_type = "pca", class = clas)

names(layer_ls)
str(layer_ls$plot_df)
str(layer_ls$decode_df)

## EXPORT OBJECTS ----
if(interactive() == TRUE){
  setwd("~/R/cheem")
  save(layer_ls,
       file = "./inst/shiny_apps/cheem_initial/data/1preprocess_penguins.RData")
}
if(F) ## Not run, load dat, clas, layer_ls
  load("./inst/shiny_apps/cheem_initial/data/1preprocess_penguins.RData")


