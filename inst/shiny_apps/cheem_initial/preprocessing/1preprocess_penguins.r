## Dependencies ------
require("cheem")

## Data preprocess ------
## Data setup, spinifex::penguins
raw <- spinifex::penguins
lvls <- levels(raw$species)
## Filter to closest 2 classes
raw <- raw[raw$species %in% lvls[1:2], ]
dat <- raw[, 1:4] %>% as.data.frame() ## X's not scaled.
colnames(dat) <- c("b_l", "b_d", "f_l", "wgt")
clas <- factor(raw$species, levels = lvls[1:2]) ## Manually remove 3rd lvl

## SHAP layer_ls -----
layer_ls <- nested_local_attr_layers(
  x = dat, y = clas, basis_type = "pca", class = clas)

names(layer_ls)
str(layer_ls$plot_df)
str(layer_ls$decode_df)

## EXPORT OBJECTS ----
if(interactive() == TRUE){
  setwd("~/R/cheem")
  save(dat,  ## penguins pre-processed data
       clas, ## penguins species
       layer_ls,
       file = "./inst/shiny_apps/cheem_initial/data/1preprocess_penguins.RData")
}
if(F){## Not run, load dat, clas, layer_ls
  load("./inst/shiny_apps/cheem_initial/data/1preprocess_penguins.RData")
}

