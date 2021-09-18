## Dependencies ------
## Local files
source("./apps/trees_of_cheem.r") ## Local functions, esp. for basis_cheem() and view_cheem()
source("./apps/cobs_n_plot_funcs.r") ## COBS func, and plotting functions for shiny
## source and apply functions, apply the work to prep for shiny app
if(F){ ## Not run, open `trees_of_cheem.r`
  file.edit("./apps/trees_of_cheem.r")
  file.edit("./apps/cobs_n_plot_funcs.r")
}

## Data simulation functions ------
source("../spinifex_study/apps_supplementary/data_simulation/_sim_user_study.r")
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
#debugonce(plot_df_of)
assign_cobs_shap_layer_ls(
  data = dat,
  class = clas,
  y = clas, ## Factor implies classification, numeric implies regression
  n_cobs = n_cobs, ## Drawn from lvl 1, assigned to other levels
  sd_coeff = .1)

names(shap_layer_ls)
str(shap_layer_ls$plot_df)
str(shap_layer_ls$decode_df)

# df <- shap_layer_ls$shap_df
# tgt_obs <- nrow(df)
# bas <- df[tgt_obs, -ncol(df)] %>%
#   as.matrix(nrow = 1L) %>% t() %>%
#   tourr::normalise()
# opts <- rownames(bas)
# sel <- opts[spinifex::manip_var_of(bas)]

# ggt <- manual_tour1d_func(
#   shap_layer_ls, basis = bas, mv_name = sel,
#   shap_obs = tgt_obs, #comp_obs = NULL,
#   do_add_pcp_segements = TRUE, 
#   pcp_shape = 124L ## ggplot '|'
# )
# anim <- animate_gganimate(ggt)
# gganimate::anim_save(
#   "cheem_manualtour.gif", animation = anim, path = ".")

## EXPORT OBJECTS ----
if(interactive() == TRUE){
  save(dat,  ## Simulation pre-processed data
       clas, ## Simulation class
       shap_layer_ls,
       file = "2preprocess_simulation.RData")
  file.copy("./2preprocess_simulation.RData", to = "./apps/cheem_app/data/2preprocess_simulation.RData", overwrite = TRUE)
  file.remove("./2preprocess_simulation.RData")
}
if(F)
{## Not run, load and review objects
  load("./apps/cheem_app/data/2preprocess_simulation.RData")
  source("./apps/trees_of_cheem.r") ## Local functions, esp. for basis_cheem() and view_cheem()
  source("./apps/cobs_n_plot_funcs.r") ## Shiny app plotting fucns
}
if(F){ ## Not run, open `trees_of_cheem.r`
  file.edit("./apps/trees_of_cheem.r")
  file.edit("./apps/cobs_n_plot_funcs.r")
}
