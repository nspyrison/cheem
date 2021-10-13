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
#cr_simulation <- function(cl_obs = 140,
#                          do_save = FALSE)
{
  cl_obs = 40; do_save = FALSE; signal_coef = 4## arguments
  set.seed(123) ## HARD CODE SEED!!!
  root <- paste0("./apps/cheem_penguins_classification/data/")
  
  ## MEANS ---
  mns_p4 <- ##______2 signal dim  | 2 noise dim
    list("cl a" = c(0, 0,           0, 0) * signal_coef,
         "cl b" = c(1, 0,           0, 0) * signal_coef,
         "cl c" = c(.5, sqrt(.75),  0, 0) * signal_coef)
  
  ## COVARIANCES ---
  sd <- 1
  ### DEFINE COMMON COVARIANCES
  #### FOR p = 4
  cov_circ_p4 <- matrix(c(sd, 0,  0,  0,
                          0,  sd, 0,  0,
                          0,  0,  sd, 0,
                          0,  0,  0,  sd),
                        ncol = 4, byrow = TRUE)
  covs_EEE_p4 <- list("cl a" = cov_circ_p4,
                      "cl b" = cov_circ_p4,
                      "cl c" = cov_circ_p4)
  
  ## SIMULATIONS ---
  #### simulate 1 draw of EEE P4, assign to global env
  assign(paste0("sim_EEE_p4"),
         this_sim_mvtnorm_cl(means = mns_p4, sigmas = covs_EEE_p4,
                             cl_obs = cl_obs),
         envir = globalenv())
  message(paste0("`", quote(sim_EEE_p4), "` has been save to the global envirnment."))
  ## _Save if needed ---
  if(do_save == TRUE){
    ## Using .rda. .rds not working b/c of long path? issue may be in the loading more than the saving.
    save(sim_EEE_p4, file = paste0(root, quote(sim_EEE_p4), ".rda"))
    message(paste0("Saved simulation to ", root, " as '", quote(sim_EEE_p4) ,".rda'. Use load('<Filename>.rda') bring obj into env. \n"))
  }
}

## Create the data & shap layer_ls -----
#cr_simulation() ## see obj `sim_EEE_p4`
dat <- sim_EEE_p4
.lvls <- LETTERS[1:3]
.clas_ints <- attr(sim_EEE_p4, "cluster") %>% as.integer()
clas <- factor(.lvls[.clas_ints], levels = .lvls)
str(sim_EEE_p4)

## Visualize
require("ggplot2")
ggplot(sim_EEE_p4, aes(V1, V2, color = clas, shape = clas)) + geom_point()

## Create shap layer_ls ---
n_cobs <- 0L
layer_ls <- assign_cobs_layer_ls(
  data = dat,
  class = clas,
  y = clas, ## Factor implies classification, numeric implies regression
  n_cobs = n_cobs, ## Drawn from lvl 1, assigned to other levels
  var_coeff = .1)

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