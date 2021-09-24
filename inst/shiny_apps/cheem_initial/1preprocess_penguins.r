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

## Create layer_ls -----
# debugonce(cheem:::local_attr_layer) ## .plot_class seems to get the corect value
# debugonce(global_view_df) ## BUt, this recieves 1...; go to <<- ?? shouldnt matter.
#debugonce(cheem:::format_nested_layers)
layer_ls <- assign_cobs_layer_ls(
  data = dat,
  class = clas,
  y = clas, ## Factor implies classification, numeric implies regression
  n_cobs = 0, ## Draw from first level, assigned to all other levels.
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
  file.copy(
    "./1preprocess_penguins.RData", overwrite = TRUE, to = 
      "./inst/shiny_apps/cheem_initial/data/1preprocess_penguins.RData")
  file.remove("./1preprocess_penguins.RData")
}
if(F){## Not run, load dat, clas, layer_ls
  load("./inst/shiny_apps/cheem_initial/data/1preprocess_penguins.RData")
}

