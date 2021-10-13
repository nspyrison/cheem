# FIFA regression, save layer_ls for app -----
## Dependencies ------
require("cheem")

## Setup ------
.raw <- DALEX::apartments ## 1000 x 6. Y | 4 Xs | 1 class
Y <- .raw[,1]
X <- .raw[,2:5]
clas <- .raw[,6]
#hist(Y) ## Slight right skew

## Create an ordered level of district; its rank of increasing sd.
require(dplyr)
.agg <- .raw %>% 
  group_by(district) %>%
  dplyr::summarise(mean_price = mean(m2.price),
                   sd_price = sd(m2.price),
                   median_price = median(m2.price)) %>%
  ungroup()
require(ggplot2)
## ~ 4 levels of sd, 3 levels of price.
if(F)
  ggplot(.agg, aes(mean_price, sd_price)) + geom_point()
.agg <- .agg %>%
  arrange(sd_price) %>%
  mutate(district_sd_rank = 1:n())
lj <- left_join(.raw, .agg, by = "district")
X <- lj[,c(2:5, 10)]

## COBS data and shap_layers -----
#### Create Courpted OBServations datasets and their shap layers.
layer_ls <- assign_cobs_layer_ls(
  data = X,
  class = clas,
  y = Y, ## Factor implies classification, numeric implies regression
  n_cobs = 0, ## Draw from first level, assigned to all other levels.
  var_coeff = .1)

names(layer_ls)
str(layer_ls$plot_df)
str(layer_ls$decode_df)

## EXPORT OBJECTS ----
if(interactive()){
  dat <- X
  save(dat,  ## non-scaled 9X aggregates of 42 var Fifa20 data.
       clas, ## district; it's sd rank is in X, but not the Y of the rf model.
       Y,    ## m2.price, price_per_sq_meter?
       layer_ls,
       file = "4preprocess_appt.RData")
  file.copy("./4preprocess_appt.RData", overwrite = TRUE, to =
  "./inst/shiny_apps/cheem_initial/data/4preprocess_appt.RData")
  file.remove("./4preprocess_appt.RData")
}
if(F){## Not run, load dat, layer_ls
  load("./inst/shiny_apps/cheem_initial/data/4preprocess_appt.RData")
}
