## Dependencies ------
## Local files
require("cheem")
## Data simulation functions ------
##TODO!! wants to be local in /R/
source("../spinifex_study/apps_supplementary/data_toy_classificiation/_sim_user_study.r")
#### esp for sim_mvtnorm_cl()

#### A simplified version
this_sim_mvtnorm_cl <- function(
  means,  ## Required
  sigmas, ## Required
  cl_obs = cl_obs) ## number of obs within each CLuster
  sim_mvtnorm_cl(means, sigmas, cl_obs = cl_obs)

#' Hard-coded wrapper function that uses mvnorm::rmvnorm() to simulate clustered data.
#'
#' @param cl_obs List, of number of observations within each cluster.
#' @param do_save logical, whether or not to save to hard coded destination.
#' @examples
#' sim_user_study(cl_obs = 140, do_save = TRUE)
#cr_simulation <- function(cl_obs = 140, signal_coef = 4)
{
  obs = 500; signal_coef = 4 ## arguments
  set.seed(123) ## HARD CODE SEED!!!
  root <- paste0("./apps/cheem_penguins_classification/data/")
  
  ## MEANS ---
  d <- diamonds
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

## Create shap layer_ls ---
### TODO: ERROR HERE: object 'y.1' not found. Rstudio won't let me select anything....
debugonce(nested_local_attr_layers)
layer_ls <- nested_local_attr_layers(X, Y, class = clas)


names(layer_ls)
str(layer_ls$plot_df)
str(layer_ls$decode_df)


## EXPORT OBJECTS ----
if(interactive() == TRUE){
  save(dat,  ## Simulation pre-processed data
       clas, ## Simulation class
       layer_ls,
       file = "2preprocess_toy_classificiation.RData")
  file.copy(
    "./2preprocess_toy_classificiation.RData", overwrite = TRUE, to =
      "./inst/shiny_apps/cheem_initial/data/2preprocess_toy_classificiation.RData")
  file.remove("./2preprocess_toy_classificiation.RData")
}
if(F){## Not run, load dat, clas, layer_ls
  load("./inst/shiny_apps/cheem_initial/data/2preprocess_toy_classificiation.RData")
}
