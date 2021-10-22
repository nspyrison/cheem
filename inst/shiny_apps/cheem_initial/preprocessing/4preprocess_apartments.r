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
X <- lj[, c(2:5, 10)]

## SHAP layer_ls -----
debugonce(nested_local_attr_layers)
#debugonce(format_nested_layers)
#debugonce(global_view_df)
layer_ls <- nested_local_attr_layers(
  x = X, y = Y, basis_type = "pca", class = clas)

names(layer_ls)


## EXPORT OBJECTS ----
if(interactive()){
  setwd("~/R/cheem")
  save(layer_ls,
       file = "./inst/shiny_apps/cheem_initial/data/4preprocess_apartments.RData")
}
if(F) ## Not run, load layer_ls
  load("./inst/shiny_apps/cheem_initial/data/4preprocess_apartments.RData")

