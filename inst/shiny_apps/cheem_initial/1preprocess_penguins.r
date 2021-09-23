## Dependencies ------
## Local files
source("./apps/trees_of_cheem.r") ## Local functions, esp. for basis_cheem() and view_cheem()
source("./apps/cobs_n_plot_funcs.r") ## COBS func, and plotting functions for shiny
## source and apply functions, apply the work to prep for shiny app
if(F){ ## Not run, open `trees_of_cheem.r`
  file.edit("./apps/trees_of_cheem.r")
  file.edit("./apps/cobs_n_plot_funcs.r")
}

## Data preprocess ------
## Data setup, spinifex::penguins
raw <- spinifex::penguins
lvls <- levels(raw$species)
## Filter to closest 2 classes
raw <- raw[raw$species %in% lvls[1:2], ]
dat <- raw[, 3:6] %>% as.data.frame() ## X's not scaled.
colnames(dat) <- c("b_l", "b_d", "f_l", "wgt")
clas <- factor(raw$species, levels = lvls[1:2]) ## Manually remove 3rd lvl

## Create layer_ls ------
layer_ls <- assign_cobs_layer_ls(
  data = dat,
  class = clas,
  y = clas, ## Factor implies classification, numeric implies regression
  n_cobs = 0, ## REALLY 5 of first lvl labeled as second, atm.
  var_coeff = .1)

names(layer_ls)
str(layer_ls$plot_df)
str(layer_ls$decode_df)

## EXPORT OBJECTS ----
if(interactive() == TRUE){
  save(dat,  ## penguins pre-processed data
       clas, ## penguins species
       layer_ls,
       file = "1preprocess_penguins.RData")
  file.copy("./1preprocess_penguins.RData", to = "./apps/cheem_app/data/1preprocess_penguins.RData", overwrite = TRUE)
  file.remove("./1preprocess_penguins.RData")
}
if(F)
{## Not run, load and review objects
  load("./apps/cheem_app/data/1preprocess_penguins.RData")
  source("./apps/trees_of_cheem.r") ## Local functions, esp. for basis_cheem() and view_cheem()
  source("./apps/cobs_n_plot_funcs.r") ## Shiny app plotting fucns
}
if(F){ ## Not run, open `trees_of_cheem.r`
  file.edit("./apps/trees_of_cheem.r")
  file.edit("./apps/cobs_n_plot_funcs.r")
}
