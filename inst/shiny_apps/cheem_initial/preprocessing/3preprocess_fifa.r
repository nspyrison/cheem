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

## SHAP layer_ls -----
layer_ls <- nested_local_attr_layers(
  x = X, y = Y, basis_type = "pca", class = clas)

names(layer_ls)


## Thin data, after model/shaps
## V2 is observed maha
.raw_layer_ls <- layer_ls ## backup
if(F){
  .maha_plot_df <- layer_ls$plot_df[
    layer_ls$plot_df$projection_nm == "QQ Mahalanobis distance",]
  ## Want to thin out lowest 90% of maha, but I don't trust the orderign of maha atm, so manual top 10%
  # hist(.maha_plot_df$V2)
  # idx_top_maha <- order(.maha_plot_df$V2, decreasing = T)
  # head(.maha_plot_df[idx_top_maha,])
  # .rownums_to_keep <- .maha_plot_df[.maha_plot_df$V2 >
  #   quantile(.maha_plot_df$V2, .9, na.rm = TRUE), 1]
  
  ## THIN: just first 500 (top skill overall/potential) (as messi and van Dijk are 1 & 8)
  .rownums_to_keep <- 1:500
  ## Order of maha numbers is not correct atm
  layer_ls$plot_df <-
    layer_ls$plot_df[layer_ls$plot_df$rownum %in% .rownums_to_keep,]
  layer_ls$decode_df <-
    layer_ls$decode_df[layer_ls$decode_df$rownum %in% .rownums_to_keep,]
  layer_ls$shap_df <- layer_ls$shap_df[.rownums_to_keep,]
}
length(.rownums_to_keep)
length(unique(.rownums_to_keep)) ## of original 5000 row nums


## EXPORT OBJECTS ----
if(interactive()){
  setwd("~/R/cheem")
  save(layer_ls,
       file = "./inst/shiny_apps/cheem_initial/data/3preprocess_fifa.RData")
}
if(F) ## Not run, load layer_ls
  load("./inst/shiny_apps/cheem_initial/data/3preprocess_fifa.RData")

