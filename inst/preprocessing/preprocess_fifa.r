## Setup ------
{
  .raw <- DALEX::fifa
  .dat_less_ys <- .raw %>%
    dplyr::select(
      -c(`nationality`, ## useless class
         ## potential target vars:
         `overall`, `potential`, `value_eur`, `wage_eur`)) %>%
    as.data.frame()
  
  if(F) ## Don't run, View corrplot of the groupings
    corrplot::corrplot(cor(.dat_less_ys),
                       method = "circle", ## geom
                       type = "upper", ## only upper triangle
                       diag = F, ## remove auto correlation
                       order = "FPC", ## First principal component
                       tl.col = "black", tl.srt = 90, ## Text label color and rotation
                       tl.pos = "td")
  
  ## Aggregate some highly correlated vars.
  dat <- .dat_less_ys %>%
    dplyr::mutate(
      .keep = "none",
      BMI = (weight_kg+(height_cm / 100)^2)/2,
      age = age,
      react = movement_reactions,
      off = (attacking_finishing+skill_long_passing+attacking_volleys+
               power_long_shots+skill_curve+mentality_positioning+attacking_crossing+
               attacking_short_passing+skill_dribbling+skill_ball_control)/10,
      def = (defending_sliding_tackle+mentality_interceptions+
               defending_standing_tackle+defending_marking+mentality_aggression)/5,
      acc = (attacking_heading_accuracy+power_shot_power)/2,
      mvm = (movement_sprint_speed+movement_balance+movement_acceleration+
               mentality_vision+mentality_composure+movement_agility+
               mentality_penalties+skill_fk_accuracy+power_stamina+movement_reactions)/10L,
      pwr = (power_strength+power_jumping)/2,
      gk  = (goalkeeping_diving+goalkeeping_positioning+goalkeeping_reflexes+
               goalkeeping_handling+goalkeeping_kicking)/5
    )
  
  ## Starting with 42 variables, we remove `nationality`, and some potential Y vars,
  #### and aggregate into 9 aggregate 'aspect' dimensions based on var correlation 
  X <- dat ## 9 aspects of the X's
  Y <- .raw$wage_eur ## wages in Euros, assumed 2020 valuation.
  clas <- dplyr::case_when(
    dat$gk <= 40 ~ "fielder",
    dat$gk >  40 ~ "goalkeeper") %>%
    factor(levels = c("fielder", "goalkeeper"))
}



rf_fit <- randomForest::randomForest(
  X, Y, ntree = 125,
  mtry = ifelse(is_discrete(Y), sqrt(ncol(X)), ncol(X) / 3),
  nodesize = max(ifelse(is_discrete(Y), 1, 5), nrow(X) / 500))
rf_pred <- predict(rf_fit, X)
rf_shap <- treeshap::treeshap(
  treeshap::randomForest.unify(rf_fit, X), X, FALSE, FALSE)
rf_shap <- rf_shap$shaps

chm <- cheem_ls(X, Y, rf_shap, rf_pred, clas,
                label = "FIFA, RF, treeshap")

## Thin data, after model/layer_ls ----
chm <- subset_cheem(chm, 1:500)

## Export ----
NM <- "preprocess_fifa.rds"
saveRDS(chm, file = paste0("~/R/cheem/inst/shiny_apps/cheem/data/", NM))
cat("Saved", NM, "\n")

if(F){
  ## Don't run load cheem list
  chm <- readRDS(paste0("./inst/shiny_apps/cheem/data/", NM))
  lapply(chm, object.size)
  
  ## Don't run manual check
  names(chm)
  global_view(chm)
}