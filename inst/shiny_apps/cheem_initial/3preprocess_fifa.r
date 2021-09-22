# FIFA regression, save shap_layer_ls for app -----

## source and apply functions, apply the work to prep for shiny app
if(F){ ## Not run, open `trees_of_cheem.r`
  file.edit("./apps/trees_of_cheem.r")
  file.edit("./apps/cobs_n_plot_funcs.r")
}

## Dependencies ------
## Local files
source("./apps/trees_of_cheem.r") ## Local functions, esp. for basis_cheem() and view_cheem()
source("./apps/cobs_n_plot_funcs.r") ## COBS func, and plotting functions for shiny

## Setup ------
.raw <- DALEX::fifa
.dat_less_ys <- .raw %>%
  dplyr::select(-c(`nationality`, ## useless class
                   `overall`, `potential`, `value_eur`, `wage_eur`)) %>% ## potential target vars.
  as.data.frame()

if(F) ## Don't run, View corrplot
  corrplot::corrplot(cor(.dat_less_ys),
                     method = "circle", ## geom
                     type = "upper", ## only upper triangle
                     diag = F, ## remove auto correlation
                     order = "FPC", ## First principal component
                     tl.col = "black", tl.srt = 90, ## Text label color and rotation
                     tl.pos = "td")

## Munging aspects -----
#### Agg some highly correlated vars.
dat <- .dat_less_ys %>%
  dplyr::mutate(
    .keep = "none",
    bdy = (weight_kg+(height_cm/100L)^2L)/2L, ## bmi wasn't working well after 01 scaling.
    age = age,
    react = movement_reactions,
    atk = (attacking_finishing+skill_long_passing+attacking_volleys+
             power_long_shots+skill_curve+mentality_positioning+attacking_crossing+
             attacking_short_passing+skill_dribbling+skill_ball_control)/10L,
    def = (defending_sliding_tackle+mentality_interceptions+
             defending_standing_tackle+defending_marking+mentality_aggression)/5L,
    acc = (attacking_heading_accuracy+power_shot_power)/2L,
    mvm = (movement_sprint_speed+movement_balance+movement_acceleration+
             mentality_vision+mentality_composure+movement_agility+
             mentality_penalties+skill_fk_accuracy+power_stamina+movement_reactions)/10L,
    pwr = (power_strength+power_jumping)/2L,
    gk = (goalkeeping_diving+goalkeeping_positioning+goalkeeping_reflexes+
            goalkeeping_handling+goalkeeping_kicking)/5L
  )
## Class for the position of the player, eiter "fielder" or "goalkeeper"
position <- clas <- dplyr::case_when(
  dat$gk <= 40L ~ "fielder",
  dat$gk >  40L ~ "goalkeeper") %>%
  factor(levels = c("fielder", "goalkeeper"))

## Starting with 42 variables, we remove `nationality`, and some potential Y vars,
#### and aggregate into 9 aggregate 'aspect' dimensions based on var correlation 
X <- dat ## 9 aspects of the X's
Y <- .raw$wage_eur ## unscaled wages in Euros, assumed 2020 valuation.

## COBS data and shap_layers -----
#### Create Courpted OBServations datasets and their shap layers.
assign_cobs_shap_layer_ls(
  data = X,
  class = clas,
  y = Y, ## Factor implies classification, numeric implies regression
  n_cobs = 0, ## Draw from first level, assigned to all other levels.
  var_coeff = .1)

names(shap_layer_ls)
str(shap_layer_ls$plot_df)
str(shap_layer_ls$decode_df)

## EXPORT OBJECTS ----
if(interactive()){
  dat <- shap_layer_ls$decode_df[, 5L:13L]
  save(dat,   ## non-scaled 9X aggregates of 42 var Fifa20 data.
       #clas, ## fielder/goal-keeper
       shap_layer_ls,
       file = "3preprocess_fifa.RData")
  file.copy("./3preprocess_fifa.RData", to = "./apps/cheem_app/data/3preprocess_fifa.RData", overwrite = TRUE)
  file.remove("./3preprocess_fifa.RData")
}
if(F)
{ ## Don't run, import
  load("./apps/cheem_app/data/3preprocess_fifa.RData")
  source("./apps/trees_of_cheem.r") ## Local functions, esp. for basis_cheem() and view_cheem()
  source("./apps/cobs_n_plot_funcs.r") ## COBS func, and plotting functions for shiny
}
if(F){ ## Not run, open `trees_of_cheem.r`
  file.edit("./apps/trees_of_cheem.r")
  file.edit("./apps/cobs_n_plot_funcs.r")
}
