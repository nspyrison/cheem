# FIFA regression, save layer_ls for app -----
## Dependencies ------
require("cheem")

## Setup ------
.raw <- DALEX::fifa
.dat_less_ys <- .raw %>%
  dplyr::select(
    -c(`nationality`, ## useless class
       ## potential target vars:
       `overall`, `potential`, `value_eur`, `wage_eur`)) %>%
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
## Class for the position of the player, either a "fielder" or "goalkeeper"
position <- clas <- dplyr::case_when(
  dat$gk <= 40L ~ "fielder",
  dat$gk >  40L ~ "goalkeeper") %>%
  factor(levels = c("fielder", "goalkeeper"))

## Starting with 42 variables, we remove `nationality`, and some potential Y vars,
#### and aggregate into 9 aggregate 'aspect' dimensions based on var correlation 
X <- dat ## 9 aspects of the X's
Y <- log(.raw$wage_eur) ## _LOG_ wages in Euros, assumed 2020 valuation.
hist(Y)

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

## Filter out lowest 80% maha distances.
## V2 is observed maha
.raw_layer_ls <- layer_ls
object.size(layer_ls)
if(F)
  layer_ls <- .raw_layer_ls
.maha_plot_df <- layer_ls$plot_df[
  layer_ls$plot_df[, "projection_nm"] == "QQ Mahalanobis distance",]
.rownums_to_keep <- .maha_plot_df[.maha_plot_df["V2"] >
  quantile(.maha_plot_df["V2"], .9, na.rm = TRUE), 1]
layer_ls$plot_df <-
  layer_ls$plot_df[layer_ls$plot_df$rownum %in% .rownums_to_keep,]
layer_ls$decode_df <-
  layer_ls$decode_df[layer_ls$decode_df$rownum %in% .rownums_to_keep,]
layer_ls$shap_df <- layer_ls$shap_df[.rownums_to_keep,]
object.size(layer_ls)
length(.rownums_to_keep)
length(unique(.rownums_to_keep)) ## of original 5000 row nums

layer_ls$decode_df <- layer_ls$decode_df[order(layer_ls$decode_df$rownum),]
if(F) ##TODO: i think the order maha dist is mukign everthing up..... will need to go back through...
  View(layer_ls$decode_df)


## EXPORT OBJECTS ----
if(interactive()){
  dat <- layer_ls$decode_df[, 5L:13L]
  save(dat,   ## non-scaled 9X aggregates of 42 var Fifa20 data.
       #clas, ## fielder/goal-keeper
       layer_ls,
       file = "3preprocess_fifa.RData")
  file.copy("./3preprocess_fifa.RData", overwrite = TRUE, to =
  "./inst/shiny_apps/cheem_initial/data/3preprocess_fifa.RData")
  file.remove("./3preprocess_fifa.RData")
}
if(F){## Not run, load dat, layer_ls
  load("./inst/shiny_apps/cheem_initial/data/3preprocess_fifa.RData")
}
