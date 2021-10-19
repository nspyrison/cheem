## Dependencies ------
## Local files
require("cheem")

{
  d <- ggplot2::diamonds
  s <- d[d$clarity == "VS1",] ## 8171
  s <- s[s$color == "J",] ##542
  s <- s[, c(7, 1, 5, 6, 8:10, 2)]
  diamonds_sub <- data.frame(log_price = log(s$price), s[,-1])
}
## Visualize
if(F)
  GGally::ggpairs(diamonds_sub, mapping = aes(color = cut))

Y <- diamonds_sub$log_price
X <- diamonds_sub[,2:7]
clas <- diamonds_sub$cut

## SHAP layer_ls -----
### TODO: ERROR HERE: object 'y.1' not found. Rstudio won't let me select anything....
debugonce(nested_local_attr_layers)
layer_ls <- nested_local_attr_layers(
  x = X, y = Y, basis_type = "pca", class = clas)

names(layer_ls)
str(layer_ls$plot_df)
str(layer_ls$decode_df)


## EXPORT OBJECTS ----
if(interactive() == TRUE){
  dat <- X
  save(dat,  ## Simulation pre-processed data
       clas, ## Simulation class
       layer_ls,
       file = "./inst/shiny_apps/cheem_initial/data/2preprocess_toy_classificiation.RData")
}
if(F){## Not run, load dat, clas, layer_ls
  load("./inst/shiny_apps/cheem_initial/data/2preprocess_toy_classificiation.RData")
}
